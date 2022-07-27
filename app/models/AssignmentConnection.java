package models;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.File;
import java.io.FileWriter;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.inject.Inject;
import javax.inject.Singleton;

import com.amazonaws.AmazonServiceException;
import com.amazonaws.auth.AWSStaticCredentialsProvider;
import com.amazonaws.auth.BasicAWSCredentials;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClientBuilder;
import com.amazonaws.services.dynamodbv2.document.DynamoDB;
import com.amazonaws.services.dynamodbv2.document.Item;
import com.amazonaws.services.dynamodbv2.document.ItemCollection;
import com.amazonaws.services.dynamodbv2.document.QueryOutcome;
import com.amazonaws.services.dynamodbv2.document.RangeKeyCondition;
import com.amazonaws.services.dynamodbv2.document.Table;
import com.amazonaws.services.dynamodbv2.document.spec.PutItemSpec;
import com.amazonaws.services.dynamodbv2.document.spec.QuerySpec;
import com.amazonaws.services.dynamodbv2.document.utils.ValueMap;
import com.amazonaws.services.dynamodbv2.model.ConditionalCheckFailedException;
import com.amazonaws.services.dynamodbv2.model.ProvisionedThroughputExceededException;
import com.amazonaws.services.dynamodbv2.model.ResourceNotFoundException;
import com.amazonaws.services.dynamodbv2.model.TransactionConflictException;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.AmazonS3ClientBuilder;
import com.amazonaws.services.s3.model.AmazonS3Exception;
import com.amazonaws.services.s3.model.ListObjectsV2Request;
import com.amazonaws.services.s3.model.ListObjectsV2Result;
import com.amazonaws.services.s3.model.ObjectMetadata;
import com.amazonaws.services.s3.model.S3ObjectSummary;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.typesafe.config.Config;

import play.Logger;

public interface AssignmentConnection {

    public boolean isOnS3(String repo);
    public boolean isOnS3(String repo, String key);
    public ObjectNode readJsonObjectFromDB(String tableName, String primaryKeyName, String primaryKeyValue) throws IOException;
    public String readJsonStringFromDB(String tableName, String primaryKeyName, String primaryKeyValue) throws IOException;
    public ObjectNode readJsonObjectFromDB(String tableName, String primaryKeyName, String primaryKeyValue, String sortKeyName, String sortKeyValue) throws IOException;
    public ObjectNode readNewestJsonObjectFromDB(String tableName, String primaryKeyName, String primaryKeyValue);
    public String readJsonStringFromDB(String tableName, String primaryKeyName, String primaryKeyValue, String sortKeyName, String sortKeyValue) throws IOException;
    public Map<String, ObjectNode> readJsonObjectsFromDB(String tableName, String primaryKeyName, String primaryKeyValue, String sortKeyName) throws IOException;
    public void writeJsonObjectToDB(String tableName, ObjectNode obj);
    public boolean writeNewerJsonObjectToDB(String tableName, ObjectNode obj, String primaryKeyName, String timeStampKeyName);

    
    public class AssignmentS3Connection implements AssignmentConnection {
        private Config config;
        private String bucketSuffix = null;
        private AmazonS3 amazonS3;
        private AmazonDynamoDB amazonDynamoDB;
        private static Logger.ALogger logger = Logger.of("com.horstmann.codecheck");
        public static class OutOfOrderException extends RuntimeException {}

        public AssignmentS3Connection(Config config) {
            this.config = config;
            String s3AccessKey = config.getString("com.horstmann.codecheck.s3.accessKey");
            String s3SecretKey = config.getString("com.horstmann.codecheck.s3.secretKey");
            String s3Region = config.getString("com.horstmann.codecheck.s3.region"); 
            // amazonS3 = AmazonS3ClientBuilder
            //         .standard()
            //         .withCredentials(new AWSStaticCredentialsProvider(new BasicAWSCredentials(s3AccessKey, s3SecretKey)))
            //         .withRegion(s3Region)
            //         .withForceGlobalBucketAccessEnabled(true)
            //         .build();
            
            amazonDynamoDB = AmazonDynamoDBClientBuilder
                    .standard()
                    .withCredentials(new AWSStaticCredentialsProvider(new BasicAWSCredentials(s3AccessKey, s3SecretKey)))
                    .withRegion("us-west-1")
                    .build();
                
            bucketSuffix = config.getString("com.horstmann.codecheck.s3bucketsuffix");            
        }

        public boolean isOnS3(String repo) {
            String key = "com.horstmann.codecheck.repo." + repo;
            return !config.hasPath(key) || config.getString(key).isEmpty();
        }
        
        public boolean isOnS3(String repo, String key) {
            String bucket = repo + "." + bucketSuffix;
            return getS3Connection().doesObjectExist(bucket, key);
        }        

        private AmazonS3 getS3Connection() { 
            return amazonS3; 
        }
    
        public AmazonDynamoDB getAmazonDynamoDB() {
            return amazonDynamoDB;
        }

        public List<String> readS3keys(String repo, String keyPrefix) throws AmazonServiceException {
            // https://docs.aws.amazon.com/AmazonS3/latest/dev/ListingObjectKeysUsingJava.html      
            String bucket = repo + "." + bucketSuffix;
            ListObjectsV2Request req = new ListObjectsV2Request()
                    .withBucketName(bucket).withMaxKeys(100).withPrefix(keyPrefix);
            ListObjectsV2Result result;
            List<String> allKeys = new ArrayList<String>();
            
            do {
                result = getS3Connection().listObjectsV2(req);
    
                for (S3ObjectSummary objectSummary : result.getObjectSummaries()) {
                    allKeys.add(objectSummary.getKey());
                }
                
                String token = result.getNextContinuationToken();
                req.setContinuationToken(token);
            } while (result.isTruncated());
            return allKeys;
        }
        
        public ObjectNode readJsonObjectFromDB(String tableName, String primaryKeyName, String primaryKeyValue) throws IOException {
            String result = readJsonStringFromDB(tableName, primaryKeyName, primaryKeyValue);
            return result == null ? null : (ObjectNode)(new ObjectMapper().readTree(result)); 
        }
        
        public String readJsonStringFromDB(String tableName, String primaryKeyName, String primaryKeyValue) throws IOException {
            DynamoDB dynamoDB = new DynamoDB(amazonDynamoDB);
            Table table = dynamoDB.getTable(tableName); 
            ItemCollection<QueryOutcome> items = table.query(primaryKeyName, primaryKeyValue);
            try {
                Iterator<Item> iterator = items.iterator();
                if (iterator.hasNext())
                    return iterator.next().toJSON();
                else
                    return null;
            } catch (ResourceNotFoundException ex) {
                return null;
            }
        }   
        
        public ObjectNode readJsonObjectFromDB(String tableName, String primaryKeyName, String primaryKeyValue, String sortKeyName, String sortKeyValue) throws IOException {
            String result = readJsonStringFromDB(tableName, primaryKeyName, primaryKeyValue, sortKeyName, sortKeyValue);
            return result == null ? null : (ObjectNode)(new ObjectMapper().readTree(result)); 
        }
        
        public ObjectNode readNewestJsonObjectFromDB(String tableName, String primaryKeyName, String primaryKeyValue) {
            DynamoDB dynamoDB = new DynamoDB(amazonDynamoDB);
            Table table = dynamoDB.getTable(tableName); 
            QuerySpec spec = new QuerySpec()
                    .withKeyConditionExpression(primaryKeyName + " = :primaryKey" )
                    .withValueMap(new ValueMap().withString(":primaryKey", primaryKeyValue))
                    .withScanIndexForward(false);
            
            ItemCollection<QueryOutcome> items = table.query(spec);
            try {
                Iterator<Item> iterator = items.iterator();
                if (iterator.hasNext()) {
                    String result = iterator.next().toJSON();
                    try {
                        return (ObjectNode)(new ObjectMapper().readTree(result));
                    } catch (JsonProcessingException ex) {
                        return null;
                    }
                }
                else
                    return null;
            } catch (ResourceNotFoundException ex) {
                return null;
            }
        }        

        public String readJsonStringFromDB(String tableName, String primaryKeyName, String primaryKeyValue, String sortKeyName, String sortKeyValue) throws IOException {
            DynamoDB dynamoDB = new DynamoDB(amazonDynamoDB);
            Table table = dynamoDB.getTable(tableName); 
            ItemCollection<QueryOutcome> items = table.query(primaryKeyName, primaryKeyValue, 
                    new RangeKeyCondition(sortKeyName).eq(sortKeyValue));
            try {
                Iterator<Item> iterator = items.iterator();
                if (iterator.hasNext())
                    return iterator.next().toJSON();
                else
                    return null;
            } catch (ResourceNotFoundException ex) {
                return null;
            }
        }

        public Map<String, ObjectNode> readJsonObjectsFromDB(String tableName, String primaryKeyName, String primaryKeyValue, String sortKeyName) throws IOException {
            DynamoDB dynamoDB = new DynamoDB(amazonDynamoDB);
            Table table = dynamoDB.getTable(tableName); 
            ItemCollection<QueryOutcome> items = table.query(primaryKeyName, primaryKeyValue);
            Iterator<Item> iterator = items.iterator();
            Map<String, ObjectNode> itemMap = new HashMap<>();
            while (iterator.hasNext()) {
                Item item = iterator.next();
                String key = item.getString(sortKeyName);
                itemMap.put(key, (ObjectNode)(new ObjectMapper().readTree(item.toJSON())));
            }
            return itemMap;
        }

        public void writeJsonObjectToDB(String tableName, ObjectNode obj) {
            DynamoDB dynamoDB = new DynamoDB(amazonDynamoDB);
            Table table = dynamoDB.getTable(tableName); 
            table.putItem(
                new PutItemSpec()
                    .withItem(Item.fromJSON(obj.toString()))
            );
        }

        public boolean writeNewerJsonObjectToDB(String tableName, ObjectNode obj, String primaryKeyName, String timeStampKeyName) {
            DynamoDB dynamoDB = new DynamoDB(amazonDynamoDB);
            Table table = dynamoDB.getTable(tableName);
                /*
        To prevent a new item from replacing an existing item, use a conditional expression that contains the attribute_not_exists function with the name of the attribute being used as the partition key for the table. Since every record must contain that attribute, the attribute_not_exists function will only succeed if no matching item exists.
        https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/SQLtoNoSQL.WriteData.html
    
        Apparently, the simpler putItem(item, conditionalExpression, nameMap, valueMap) swallows the ConditionalCheckFailedException 
                 */
            String conditionalExpression = "attribute_not_exists(" + primaryKeyName + ") OR " + timeStampKeyName + " < :" + timeStampKeyName;
            try {
                table.putItem(
                    new PutItemSpec()
                        .withItem(Item.fromJSON(obj.toString()))
                        .withConditionExpression(conditionalExpression)
                        .withValueMap(Collections.singletonMap(":" + timeStampKeyName, obj.get(timeStampKeyName).asText())));
                return true;
            } catch(ConditionalCheckFailedException e) {
                // https://github.com/aws/aws-sdk-java/issues/1945
                logger.warn("writeNewerJsonObjectToDynamoDB: " + e.getMessage() + " " + obj);
                return false;
            }   
        }
    }

    public class AssignmentLocalConnection implements AssignmentConnection {
        public boolean isOnS3(String repo) {
            return false;
        }
        public boolean isOnS3(String repo, String key) {
            return false;
        }
        public ObjectNode readJsonObjectFromDB(String tableName, String primaryKeyName, String primaryKeyValue) throws IOException {return null;}
        public String readJsonStringFromDB(String tableName, String primaryKeyName, String primaryKeyValue) throws IOException {return null;}
        public ObjectNode readJsonObjectFromDB(String tableName, String primaryKeyName, String primaryKeyValue, String sortKeyName, String sortKeyValue) throws IOException {
            return null;
        }
        public ObjectNode readNewestJsonObjectFromDB(String tableName, String primaryKeyName, String primaryKeyValue) {return null;}
        public String readJsonStringFromDB(String tableName, String primaryKeyName, String primaryKeyValue, String sortKeyName, String sortKeyValue) throws IOException {
            return null;
        }
        public Map<String, ObjectNode> readJsonObjectsFromDB(String tableName, String primaryKeyName, String primaryKeyValue, String sortKeyName) throws IOException {
            return null;
        }
        public void writeJsonObjectToDB(String tableName, ObjectNode obj) {
            
        }
        public boolean writeNewerJsonObjectToDB(String tableName, ObjectNode obj, String primaryKeyName, String timeStampKeyName) {
            return false;
        }
    }

    @Singleton class AssignmentConnector implements AssignmentConnection {
        private AssignmentConnection delegate;

        @Inject public AssignmentConnector(Config config) {
            if (!config.hasPath("com.horstmann.codecheck.s3.accessKey")) {
                delegate = new AssignmentLocalConnection();
            }
            else
                delegate = new AssignmentS3Connection(config);
        }

        public boolean isOnS3(String repo) {
            return false;
        }
        public boolean isOnS3(String repo, String key) {
            return false;
        }
        public ObjectNode readJsonObjectFromDB(String tableName, String primaryKeyName, String primaryKeyValue) throws IOException {
            return delegate.readJsonObjectFromDB(tableName, primaryKeyName, primaryKeyValue);
        }
        public String readJsonStringFromDB(String tableName, String primaryKeyName, String primaryKeyValue) throws IOException {
            return delegate.readJsonStringFromDB(tableName, primaryKeyName, primaryKeyValue);
        }
        public ObjectNode readJsonObjectFromDB(String tableName, String primaryKeyName, String primaryKeyValue, String sortKeyName, String sortKeyValue) throws IOException {
            return delegate.readJsonObjectFromDB(tableName, primaryKeyName, primaryKeyValue, sortKeyName, sortKeyValue);
        }
        public ObjectNode readNewestJsonObjectFromDB(String tableName, String primaryKeyName, String primaryKeyValue) {
            return delegate.readNewestJsonObjectFromDB(tableName, primaryKeyName, primaryKeyValue);
        }
        public String readJsonStringFromDB(String tableName, String primaryKeyName, String primaryKeyValue, String sortKeyName, String sortKeyValue) throws IOException {
            return delegate.readJsonStringFromDB(tableName, primaryKeyName, primaryKeyValue, sortKeyName, sortKeyValue);
        }
        public Map<String, ObjectNode> readJsonObjectsFromDB(String tableName, String primaryKeyName, String primaryKeyValue, String sortKeyName) throws IOException {
            return delegate.readJsonObjectsFromDB(tableName, primaryKeyName, primaryKeyValue, sortKeyName);
        }
        public void writeJsonObjectToDB(String tableName, ObjectNode obj) {
            delegate.writeJsonObjectToDB(tableName, obj);
        }
        public boolean writeNewerJsonObjectToDB(String tableName, ObjectNode obj, String primaryKeyName, String timeStampKeyName) {
            return delegate.writeNewerJsonObjectToDB(tableName, obj, primaryKeyName, timeStampKeyName);
        }        
    }
}
