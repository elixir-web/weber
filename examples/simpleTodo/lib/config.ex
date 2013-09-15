defmodule Config do 

    def config do
        [webserver: 
            [http_host: "localhost", 
             http_port: 8080,
             acceptors: 100
            ]
        ]
    end

end
