import ast

# Adds a logging expression to the beginning of a function definition.
def add_logging(func_string, log_expression):
    # Parse the function string into an AST
    func_ast = ast.parse(func_string)

    # Assuming the function is the first node in the module
    func_def = func_ast.body[0]
    if not isinstance(func_def, ast.FunctionDef):
        raise ValueError("Input must be a valid function definition.")

    # Insert the log expression at the beginning of the function body
    func_def.body.insert(0, log_expression)
    # Convert the modified AST back to code
    transformed_code = ast.unparse(func_ast)

    return transformed_code

# Original function as a string
foo_string = """
def foo(x):
    print(x)
    return x + x
"""

# Create a generic logging expression
inject_code = ast.Expr(
    value=ast.Call(
        func=ast.Name(id="print", ctx=ast.Load()),
        args=[ast.Constant(value="Executing foo")],
        keywords=[]
    )
)

# Add logging to the function
transformed_fn = add_logging(foo_string, inject_code)

# Show the code for debugging
print(transformed_fn)

# Evaluate the transformed function definition
exec(transformed_fn)

# Test the transformed function.
# Executing foo
# 10
# 20
print(foo(10))
