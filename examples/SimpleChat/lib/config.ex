defmodule Config do 

    def config do
        [webserver: 
            [http_host: "localhost", 
             http_port: 8080,
             acceptors: 100,
             ssl: false,
             cacertfile_path: "",
             certfile_path: "",
             keyfile_path: "",

             ws: true,
             ws_port: 8800,
             ws_mod: :Simplechat.Main.Chat 
            ]
        ]
    end

end
