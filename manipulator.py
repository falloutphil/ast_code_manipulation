import ast

def add_logging(func_string, log_message):
    # Parse the function string into an AST
    func_ast = ast.parse(func_string)

    # Assuming the function is the first node
    func_def = func_ast.body[0]

    # Create a print statement
    print_stmt = ast.Expr(
        value=ast.Call(
            func=ast.Name(id='print', ctx=ast.Load()),
            args=[ast.Constant(value=log_message)],
            keywords=[]
        )
    )

    # Insert the print statement at the beginning of the function body
    func_def.body.insert(0, print_stmt)

    # Convert the modified AST back to code
    transformed_code = ast.unparse(func_ast)

    return transformed_code

# Original function as a string
foo_string = """
def foo(x):
    print(x)
    return x + x
"""

# Add logging to the function
transformed_fn = add_logging(foo_string, "Executing foo")

# Display the transformed function
print(transformed_fn)

# Evaluate the transformed function
exec(transformed_fn)

# Test the transformed function
foo(10)  # Should print "Executing foo" and then "10" and return 20
