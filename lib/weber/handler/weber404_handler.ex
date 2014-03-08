defmodule Handler.Weber404Handler do
    
  def get404 do
    """
      <!DOCTYPE html>
        <html>
          <head>
            <meta charset="UTF-8" />
            <title>Page not found</title>
          </head>
 
         <body>
           <h1>Page not found</h1>
         </body>
      </html>
    """
  end

end