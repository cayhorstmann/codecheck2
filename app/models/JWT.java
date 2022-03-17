package models;

import java.time.Duration;
import java.time.Instant;
import java.util.Date;
import java.util.Map;

import javax.inject.Inject;
import javax.inject.Singleton;

import com.typesafe.config.Config;

import io.jsonwebtoken.JwtException;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;

@Singleton
public class JWT {
    private String key;
    @Inject public JWT(Config config) {
        key = config.getString("play.http.secret.key");
    }
    public String generate(Map<String, Object> claims) {
        String jwt = Jwts.builder()
            .setIssuer("codecheck.io")
            .addClaims(claims)
            .setIssuedAt(Date.from(Instant.now()))
            .setExpiration(Date.from(Instant.now().plus(Duration.ofDays(7))))
            .signWith(SignatureAlgorithm.HS256, key)
            .compact();
        return jwt;
    }
    
    // TODO: Do we need to verify it's signed with the same algorithm?
    public Map<String, Object> verify(String token) {
        try {
            return Jwts.parser()
                .setSigningKey(key)
                .parseClaimsJws(token)
                .getBody();
        } catch (JwtException e) {
            return null;
        }
    }
}
