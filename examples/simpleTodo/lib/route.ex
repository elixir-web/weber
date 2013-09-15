defmodule Route do

    import Weber.Route

    @route on("/", :Simpletodo.Main, :action)
        |> on("/add/:note", :Simpletodo.Main, :add)

    def get_route do
        @route
    end
end
