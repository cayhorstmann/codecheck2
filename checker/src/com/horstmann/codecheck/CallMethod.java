package com.horstmann.codecheck;

import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicBoolean;

public class CallMethod {
    private boolean outcome;
    private List<Object> expectedRets = new ArrayList<>();
    private Properties props;
    private String className;
    private double tolerance = 1.0E-6;
    private int timeoutMillis;
    private boolean ignoreCase = true;
    private boolean ignoreSpace = true;    

    public CallMethod(String className, Properties props, int timeoutMillis) {
        this.className = className;
        this.props = props;
        this.timeoutMillis = timeoutMillis;
    }
    
    public void setTolerance(double tolerance) {
		this.tolerance = tolerance;
	}
    
    public void setIgnoreCase(boolean ignoreCase) {
		this.ignoreCase = ignoreCase;
	}
    
    public void setIgnoreSpace(boolean ignoreSpace) {
		this.ignoreSpace = ignoreSpace;
	}

    public boolean getOutcome() {
        return outcome;
    }

    String getProperty(String... keys) {
        for (String k : keys) {
            String value = props.getProperty(k);
            if (value != null)
                return value;
        }
        return null;
    }

    public void prepare(Path dir) throws Exception {
        URLClassLoader loader = new URLClassLoader(new URL[] { dir.toUri().toURL() });
        loader.setDefaultAssertionStatus(true);
        // TODO: equals will fail if this compilation involves supplied classes
        // since this class loader is different than the one with which the
        // student submission is loaded
        try {
            Class<?> cl = loader.loadClass(className);
            int i = 1;
            while (true) {
                List<String> paramValues = getParameterValues(i);
                if (paramValues == null) return;
                Object ret = null;
                try {
                    ret = callMethod(cl, i, paramValues);
                } catch (Throwable t) {
                }
                expectedRets.add(ret);
                i++;
            }
        } finally {
            loader.close();
        }
    }

    @SuppressWarnings("deprecation")
    private Object callMethod(Class<?> cl, int i, List<String> paramValues) throws Throwable {
        final Method method = getMethod(cl, i);
        final Object obj = (method.getModifiers() & Modifier.STATIC) == 0 ? cl.newInstance() : null;

        Class<?>[] paramTypes = method.getParameterTypes();
        final Object[] methodArgs = new Object[paramValues.size()];
        for (int j = 0; j < paramValues.size(); j++)
            methodArgs[j] = convert(paramValues.get(j), paramTypes[j]);
        
        final Object[] result = { null };
        
        final AtomicBoolean done = new AtomicBoolean(false);
        final Thread mainmethodThread = new Thread() {
            public void run() {
                try {
                	result[0] = method.invoke(obj, methodArgs);
                } catch (Throwable t) {
                    result[0] = t;
                }
                done.set(true);
            }
        };

        mainmethodThread.start();

        try {
            mainmethodThread.join(timeoutMillis);
        } catch (InterruptedException e) {
        }
        if (!done.get()) {
            mainmethodThread.stop();        
            throw new RuntimeException("Timed out after " 
            		+ (timeoutMillis >= 2000 ? timeoutMillis / 1000 + " seconds" 
            				: timeoutMillis + " milliseconds"));
        }
        
        if (result[0] instanceof Throwable) throw (Throwable) result[0]; // TODO: Really? Do this upstream?
        else return result[0];
    }

    public void run(Path dir, Report report, Score score) throws Exception {
        URLClassLoader loader = new URLClassLoader(new URL[] { dir.toUri().toURL() });
        loader.setDefaultAssertionStatus(true);

        report.header("Testing method " + getMethodName());

        int n = size();
        String[][] args = new String[n][1];
        String[] actual = new String[n];
        String[] expected = new String[n];
        boolean[] outcomes = new boolean[n];

        try {
            Class<?> cl = loader.loadClass(className);

            outcome = true;
            for (int i = 1; i <= n; i++) {
                List<String> paramValues = getParameterValues(i);
                Object actualRet = null;
                Throwable thrown = null;
                try {
                    actualRet = callMethod(cl, i, paramValues);
                } catch (InvocationTargetException ex) {
                    thrown = ex.getCause();
                } catch (Throwable t) {
                    thrown = t;
                }

                StringBuilder builder = new StringBuilder();
                for (int k = 0; k < paramValues.size(); k++) {
                    if (k > 0)
                        builder.append(',');
                    builder.append(paramValues.get(k));
                }
                args[i][0] = builder.toString();

                boolean pass = false;
                Object expectedRet = expectedRets.get(i - 1);
                if (thrown == null) {
                	pass = compare(actualRet, expectedRet);
                }
                if (thrown == null)
                	actual[i] = toString(actualRet);
                else
                    actual[i] = thrown.getClass().getName();
                expected[i] = toString(expectedRet);
                score.pass(pass);
                outcomes[i] = pass;
                if (!pass)
                    outcome = false;
            }
        } finally {
            loader.close();
        }
        report.runTable(new String[] { "Arguments" }, args, actual, expected, outcomes);
    }
    
    private boolean compare(Object a, Object b) {
        if (a == null)
            return a == b;
        else if (a.getClass().isArray() && b.getClass().isArray()) { 
        	int alen = Array.getLength(a);
        	if (alen != Array.getLength(b)) return false;
        	for (int i = 0; i < alen; i++) {
        		if (!(compare(Array.get(a, i), Array.get(b, i)))) return false;
        	}
            return true;
        }
        else if (a instanceof Number && b instanceof Number) {
        	return Math.abs(((Number) a).doubleValue() - ((Number) b).doubleValue()) <= tolerance;
        }
        else if (a instanceof String && b instanceof String) {
        	if (ignoreSpace) {
        		a = ((String) a).replaceAll("\\s+", " ").trim();
        		b = ((String) b).replaceAll("\\s+", " ").trim();
        	}
        	if (ignoreCase) 
        		return ((String) a).equalsIgnoreCase((String) b);
        	else
        		return a.equals(b);
        }
        else
            return a.equals(b);    	
    }

    private Object convert(String value, Class<?> cl) {
        if (value.equals("null"))
            return null;
        if (cl == String.class)
            return value;

        if (cl.isArray()) {
            value = value.trim();
            if (!(value.startsWith("[") && value.endsWith("]")))
                throw new IllegalArgumentException("array values must be delimited by [ ]" + value);
            value = value.substring(1, value.length() - 1).trim();

            ArrayList<Integer> commas = new ArrayList<Integer>();
            int level = 0;
            commas.add(-1);
            for (int i = 0; i < value.length(); i++) {
                char ch = value.charAt(i);
                if (ch == '[')
                    level++;
                else if (ch == ']')
                    level--;
                else if (ch == ',' && level == 0)
                    commas.add(i);
            }
            int length = value.length() == 0 ? 0 : commas.size(); // [] has
            // length 0,
            // [1] has
            // length 1
            commas.add(value.length());
            String[] values = new String[length];
            for (int i = 0; i < length; i++) {
                values[i] = value.substring(commas.get(i) + 1, commas.get(i + 1)).trim();
            }
            Object array = Array.newInstance(cl.getComponentType(), length);
            for (int i = 0; i < length; i++) {
                Array.set(array, i, convert(values[i], cl.getComponentType()));
            }
            return array;
        }

        // Turn primitives into wrapper classes
        Class<?> cl1 = cl;
        if (cl == int.class)
            cl1 = Integer.class;
        else if (cl == byte.class)
            cl1 = Byte.class;
        else if (cl == short.class)
            cl1 = Short.class;
        else if (cl == long.class)
            cl1 = Long.class;
        else if (cl == float.class)
            cl1 = Float.class;
        else if (cl == double.class)
            cl1 = Double.class;
        else if (cl == boolean.class)
            cl1 = Boolean.class;
        else if (cl == char.class) {
            return new Character(value.charAt(0));
        }

        // Check if there is a constructor from a string
        try {
            Constructor<?> cons = cl1.getConstructor(String.class);
            return cons.newInstance(value);
        } catch (Exception ex) {
            // We tried
        }

        // Static constants

        int index = value.lastIndexOf(".");
        if (index != -1) {
            try {
                Class<?> vcl = Class.forName(value.substring(0, index));
                return vcl.getField(value.substring(index + 1)).get(null);
            } catch (Exception ex) {
                // We tried
            }
        }

        // TODO: If the type is a collection, we could handle
        // the common cases of integer, double, and string elements

        throw new IllegalArgumentException("Cannot convert " + value + " to " + cl.getName());
    }

    private static String toString(Object obj) {
        if (obj == null)
            return "null";
        Class<?> cl = obj.getClass();
        if (cl.isArray()) {
            String r = "[";
            for (int i = 0; i < Array.getLength(obj); i++) {
                if (i > 0)
                    r += ", ";
                r += toString(Array.get(obj, i));
            }
            return r + "]";
        } else
            return "" + obj;
    }

    private String getMethodName() {
        return getProperty("method", "test.method"); // Legacy
    }

    private Method getMethod(Class<?> cl, int i) {
        String methodName = getMethodName();
        if (methodName == null) throw new IllegalArgumentException("no test.method");
        Method method = null;
        for (Method m : cl.getMethods()) {
            if (m.getName().equals(methodName)) {
                if (method != null)
                    throw new IllegalArgumentException("more than one method " + className + "." + methodName);
                method = m;
            }
        }
        if (method == null)
            throw new IllegalArgumentException("method " + className + "." + methodName + " not found");
        return method;
    }

    private List<String> getParameterValues(int i) {
        List<String> values = new ArrayList<String>();
        int j = 1;
        String value;
        while ((value = getProperty("test" + i + ".param" + j, "test." + i + ".param." + j)) != null) {
            values.add(value);
            j++;
        }
        if (j == 1 && getProperty("test." + i + ".param") == null) // Legacy?
            return null;
        else
            return values;
    }

    public int size() {
        int i = 1;
        while (getProperty("test" + i + ".param" + 1, "test." + i + ".param." + 1) != null) {
            i++;
        }
        return i - 1;
    }
}
