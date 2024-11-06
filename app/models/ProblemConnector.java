package models;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.*;
import java.util.Arrays;

import javax.inject.Inject;
import javax.inject.Singleton;

import com.amazonaws.auth.AWSStaticCredentialsProvider;
import com.amazonaws.auth.BasicAWSCredentials;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.AmazonS3ClientBuilder;
import com.amazonaws.services.s3.model.AmazonS3Exception;
import com.amazonaws.services.s3.model.ObjectMetadata;
import com.typesafe.config.Config;

import play.Logger;
import play.db.Database;
import play.db.Databases;

// TODO Use DI configuration to avoid this delegation

@Singleton
public class ProblemConnector {
    private ProblemConnection delegate;

    @Inject public ProblemConnector(Config config /*, Database db */) {
        if (config.hasPath("com.horstmann.codecheck.s3.region"))
            delegate = new ProblemS3Connection(config);   
        else if (config.getBoolean("com.horstmann.codecheck.sql")) {
        	Database db = Databases.createFrom("org.postgresql.Driver", "postgres://5ulduk:xau_nODZPZpCPgD43abKvOco9y1Lv9HQeMm51@us-east-1.sql.xata.sh/codeday:main");
            delegate = new ProblemSQLConnection(db);
        }
        else 
            delegate = new ProblemLocalConnection(config);
    }

    public byte[] readProblem(String repo, String key) throws IOException {
        return delegate.readProblem(repo, key);
    }

    public void writeProblem(byte[] contents, String repo, String key) throws IOException {
        delegate.writeProblem(contents, repo, key);
    }
}

interface ProblemConnection {
    public byte[] readProblem(String repo, String key) throws IOException;
    public void writeProblem(byte[] contents, String repo, String key) throws IOException;
}

class ProblemS3Connection implements ProblemConnection {
    private String bucketSuffix = null;
    private AmazonS3 amazonS3;
    private static Logger.ALogger logger = Logger.of("com.horstmann.codecheck");

    public ProblemS3Connection(Config config) {
        String awsAccessKey = config.getString("com.horstmann.codecheck.aws.accessKey");
        String awsSecretKey = config.getString("com.horstmann.codecheck.aws.secretKey");
        String region = config.getString("com.horstmann.codecheck.s3.region"); 
        amazonS3 = AmazonS3ClientBuilder
                .standard()
                .withCredentials(new AWSStaticCredentialsProvider(new BasicAWSCredentials(awsAccessKey, awsSecretKey)))
                .withRegion(region)
                .withForceGlobalBucketAccessEnabled(true)
                .build();

        bucketSuffix = config.getString("com.horstmann.codecheck.s3.bucketsuffix");
    }

    public byte[] readProblem(String repo, String key) throws IOException {
        String bucket = repo + "." + bucketSuffix;

        byte[] bytes = null;
        try {
            // TODO -- trying to avoid warning 
            // WARN - com.amazonaws.services.s3.internal.S3AbortableInputStream - Not all bytes were read from the S3ObjectInputStream, aborting HTTP connection. This is likely an error and may result in sub-optimal behavior. Request only the bytes you need via a ranged GET or drain the input stream after use
            try (InputStream in = amazonS3.getObject(bucket, key).getObjectContent()) {
                bytes = in.readAllBytes();
            }
        } catch (AmazonS3Exception ex) {
            logger.error("S3Connection.readFromS3: Cannot read " + key + " from " + bucket);
            throw ex;
        }
        return bytes;            
    }

    public void writeProblem(byte[] contents, String repo, String key) throws IOException {
        String bucket = repo + "." + bucketSuffix;
        ObjectMetadata metadata = new ObjectMetadata();
        metadata.setContentLength(contents.length);
        metadata.setContentType("application/zip");
        try {
            try (ByteArrayInputStream in = new ByteArrayInputStream(contents)) {
                amazonS3.putObject(bucket, key, in, metadata); 
            }                 
        } catch (AmazonS3Exception ex) {
            String bytes = Arrays.toString(contents);
            logger.error("S3Connection.putToS3: Cannot put " + bytes.substring(0, Math.min(50, bytes.length())) + "... to " + bucket);
            throw ex;                
        }
    }
}

class ProblemLocalConnection implements ProblemConnection {
    private Path root;
    private static Logger.ALogger logger = Logger.of("com.horstmann.codecheck");

    public ProblemLocalConnection(Config config) {
        this.root = Path.of(config.getString("com.horstmann.codecheck.s3.local"));
        try {
           Files.createDirectories(root);            
        } catch (IOException ex) {
            logger.error("Cannot create " + root);
        } 
    }

    public byte[] readProblem(String repo, String key) throws IOException {
        byte[] result = null;
        try {
            Path repoPath = root.resolve(repo);
            Path filePath = repoPath.resolve(key + ".zip");
            result = Files.readAllBytes(filePath); 
        } catch (IOException ex) {
            logger.error("ProblemLocalConnection.read : Cannot read " + key + " from " + repo);
            throw ex;                
        }
        
        return result;  
    }

    public void writeProblem(byte[] contents, String repo, String key) throws IOException {
        try {
            Path repoPath = root.resolve(repo);
            Files.createDirectories(repoPath);                
            Path newFilePath = repoPath.resolve(key + ".zip");
            Files.write(newFilePath, contents);
        } catch (IOException ex) {
            String bytes = Arrays.toString(contents);
            logger.error("ProblemLocalConnection.write : Cannot put " + bytes.substring(0, Math.min(50, bytes.length())) + "... to " + repo);
            throw ex;                   
        }

    }
}

/*

CREATE TABLE Problems (repo VARCHAR, key VARCHAR, contents BYTEA, UNIQUE (repo, key))

*/

class ProblemSQLConnection implements ProblemConnection {
    private static Logger.ALogger logger = Logger.of("com.horstmann.codecheck");
    private Database db;
    public ProblemSQLConnection(Database db) {
        this.db = db; // TODO
    }

    public byte[] readProblem(String repo, String key) throws IOException {
        try (Connection conn = db.getConnection()) {
            PreparedStatement ps = conn.prepareStatement("SELECT contents FROM Problems WHERE repo = ? AND key = ?");
            ps.setString(1, repo);
            ps.setString(2, key);
            ResultSet rs = ps.executeQuery();
            if (rs.next()) return rs.getBytes(1);
            else return null;
        } catch (SQLException ex) {
            logger.error(ex.getMessage());
            throw new IOException(ex);
        }
    }

    public void writeProblem(byte[] contents, String repo, String key) throws IOException {
        try {
            try (Connection conn = db.getConnection()) {
            	// https://www.postgresql.org/docs/current/ddl-constraints.html#DDL-CONSTRAINTS-UNIQUE-CONSTRAINTS
                PreparedStatement ps = conn.prepareStatement("""
INSERT INTO Problems VALUES (?, ?, ?) 
ON CONFLICT (repo, key) 
DO UPDATE SET contents = EXCLUDED.contents                		
""");
                ps.setString(1, repo);
                ps.setString(2, key);
                ps.setBytes(3, contents);
                ps.executeUpdate();
            }
        } catch (SQLException ex) {
            logger.error(ex.getMessage());
            throw new IOException(ex);
        }
    }
}

