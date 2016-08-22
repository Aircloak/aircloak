defmodule BOM.Gather do
  def node(path), do:
    BOM.Gather.Node.run(path)
end
