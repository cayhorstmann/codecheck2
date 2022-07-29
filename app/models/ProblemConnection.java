package models;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.File;
import java.io.FileWriter;
import java.nio.file.Path;
import java.nio.file.Files;
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

public interface ProblemConnection {

    // May not need
    public boolean isOnS3(String repo);
    public boolean isOnS3(String repo, String key);
    // May not need
    public void write(Path file, String repo, String key) throws IOException;
    public void write(String contents, String repo, String key) throws IOException;
    public void write(byte[] contents, String repo, String key) throws IOException;
    public void delete(String repo, String key) throws IOException;
    public byte[] read(String repo, String problem) throws IOException;


    public class ProblemS3Connection implements ProblemConnection {
        private Config config;
        private String bucketSuffix = null;
        private AmazonS3 amazonS3;
        // private AmazonDynamoDB amazonDynamoDB; - Part of AssignmentConnection
        private static Logger.ALogger logger = Logger.of("com.horstmann.codecheck");

        public ProblemS3Connection(Config config) {
            this.config = config;
            String s3AccessKey = config.getString("com.horstmann.codecheck.s3.accessKey");
            String s3SecretKey = config.getString("com.horstmann.codecheck.s3.secretKey");
            String s3Region = config.getString("com.horstmann.codecheck.s3.region"); 
            amazonS3 = AmazonS3ClientBuilder
                    .standard()
                    .withCredentials(new AWSStaticCredentialsProvider(new BasicAWSCredentials(s3AccessKey, s3SecretKey)))
                    .withRegion(s3Region)
                    .withForceGlobalBucketAccessEnabled(true)
                    .build();
            
            // amazonDynamoDB = AmazonDynamoDBClientBuilder   
            //         .standard()
            //         .withCredentials(new AWSStaticCredentialsProvider(new BasicAWSCredentials(s3AccessKey, s3SecretKey)))
            //         .withRegion("us-west-1")
            //         .build();

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

        // public AmazonDynamoDB getAmazonDynamoDB() {
        //     return amazonDynamoDB;
        // }
        
        public void write(Path file, String repo, String key) throws IOException {
            String bucket = repo + "." + bucketSuffix;
            try {
                getS3Connection().putObject(bucket, key, file.toFile());
            } catch (AmazonS3Exception ex) {
                logger.error("S3Connection.putToS3: Cannot put " + file + " to " + bucket);
                throw ex;
            }
        }

        public void write(String contents, String repo, String key) throws IOException {
            String bucket = repo + "." + bucketSuffix;
            try {
                getS3Connection().putObject(bucket, key, contents);
            } catch (AmazonS3Exception ex) {
                logger.error("S3Connection.putToS3: Cannot put " + contents.replaceAll("\n", "|").substring(0, Math.min(50, contents.length())) + "... to " + bucket);
                throw ex;
            }
        }

        public void write(byte[] contents, String repo, String key) throws IOException {
            String bucket = repo + "." + bucketSuffix;
            ObjectMetadata metadata = new ObjectMetadata();
            metadata.setContentLength(contents.length);
            metadata.setContentType("application/zip");
            try {
                try (ByteArrayInputStream in = new ByteArrayInputStream(contents)) {
                    getS3Connection().putObject(bucket, key, in, metadata); 
                }                 
            } catch (AmazonS3Exception ex) {
                String bytes = Arrays.toString(contents);
                logger.error("S3Connection.putToS3: Cannot put " + bytes.substring(0, Math.min(50, bytes.length())) + "... to " + bucket);
                throw ex;                
            }
        }

        public void delete(String repo, String key) throws IOException {
            String bucket = repo + "." + bucketSuffix;
            try {
               getS3Connection().deleteObject(bucket, key);
            } catch (AmazonS3Exception ex) {
                logger.error("S3Connection.deleteFromS3: Cannot delete " + bucket);
                throw ex;
            }            
        }

        public byte[] read(String repo, String problem) throws IOException {
            String bucket = repo + "." + bucketSuffix;

            byte[] bytes = null;
            try {
                // TODO -- trying to avoid warning 
                // WARN - com.amazonaws.services.s3.internal.S3AbortableInputStream - Not all bytes were read from the S3ObjectInputStream, aborting HTTP connection. This is likely an error and may result in sub-optimal behavior. Request only the bytes you need via a ranged GET or drain the input stream after use
                try (InputStream in = getS3Connection().getObject(bucket, problem).getObjectContent()) {
                    bytes = in.readAllBytes();
                }
            } catch (AmazonS3Exception ex) {
                logger.error("S3Connection.readFromS3: Cannot read " + problem + " from " + bucket);
                throw ex;
            }
            return bytes;            
        }

        // public List<String> readS3keys(String repo, String keyPrefix) throws AmazonServiceException {
        //     // https://docs.aws.amazon.com/AmazonS3/latest/dev/ListingObjectKeysUsingJava.html      
        //     String bucket = repo + "." + bucketSuffix;
        //     ListObjectsV2Request req = new ListObjectsV2Request()
        //             .withBucketName(bucket).withMaxKeys(100).withPrefix(keyPrefix);
        //     ListObjectsV2Result result;
        //     List<String> allKeys = new ArrayList<String>();
            
        //     do {
        //         result = getS3Connection().listObjectsV2(req);

        //         for (S3ObjectSummary objectSummary : result.getObjectSummaries()) {
        //             allKeys.add(objectSummary.getKey());
        //         }

        //         String token = result.getNextContinuationToken();
        //         req.setContinuationToken(token);
        //     } while (result.isTruncated());
        //     return allKeys;            
        // }

    }

    public class ProblemLocalConnection implements ProblemConnection {
        // May not need
        public boolean isOnS3(String repo) {return false;}
        public boolean isOnS3(String repo, String key) {return false;}
        // May not need
        private Config config;
        private String bucketSuffix = null;
        public ProblemLocalConnection(Config config) {
            this.config = config;
        }
        public void write(Path file, String repo, String key) throws IOException {}
        public void write(String contents, String repo, String key) throws IOException {}
        public void write(byte[] contents, String repo, String key) throws IOException {
            Path repoPath = Path.of(config.getString("com.horstmann.codecheck.repo." + repo));
            Path problemDir = repoPath.resolve(key);
            com.horstmann.codecheck.Util.deleteDirectory(problemDir); // Delete any prior contents so that it is replaced by new zip file
            Files.createDirectories(problemDir);
            problemDir = problemDir.resolve("problem.zip");
            org.apache.commons.io.FileUtils.writeByteArrayToFile(new File(problemDir.toString()), contents);
        }
        public void delete(String repo, String key) throws IOException {}
        public byte[] read(String repo, String problem) throws IOException {
            return null;
        }
    }

    @Singleton class ProblemConnector implements ProblemConnection {
        private ProblemConnection delegate;

        @Inject public ProblemConnector(Config config) {
            if (!config.hasPath("com.horstmann.codecheck.s3.accessKey")) {
                delegate = new ProblemLocalConnection(config);
            }
            else
                delegate = new ProblemS3Connection(config);
        }

        public boolean isOnS3(String repo) {
            return delegate.isOnS3(repo);
        }

        public boolean isOnS3(String repo, String key) {
            return delegate.isOnS3(repo, key);
        }
        public void write(Path file, String repo, String key) throws IOException {
            delegate.write(file, repo, key);
        }
        public void write(String contents, String repo, String key) throws IOException {
            delegate.write(contents, repo, key);
        }
        public void write(byte[] contents, String repo, String key) throws IOException {
            delegate.write(contents, repo, key);
        }
        public void delete(String repo, String key) throws IOException {
            delegate.delete(repo, key);
        }
        public byte[] read(String repo, String problem) throws IOException {
            return delegate.read(repo, problem);
        }
    }


}
