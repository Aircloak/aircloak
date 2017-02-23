defmodule Central.ImportController do
  @moduledoc false
  use Central.Web, :controller
  require Logger


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def new(conn, _params), do:
    render(conn)

  def create(conn, params) do
    with {:ok, export_data} <- read_uploaded_file(params),
         {:ok, imported_count} <- import_data(export_data) do
      conn
      |> put_flash(:info, "Imported #{imported_count} entries.")
      |> redirect(to: customer_path(conn, :index))
    else
      {:error, message} ->
        conn
        |> put_flash(:error, message)
        |> redirect(to: import_path(conn, :new))
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp read_uploaded_file(params) do
    case params["import"]["file"] do
      nil -> {:error, "Please provide the import file!"}
      upload -> {:ok, File.read!(upload.path)}
    end
  end

  defp import_data(export_data) do
    case Central.CustomerMessage.import_customer_data(export_data) do
      {:error, reason} ->
        Logger.error("Error importing customer data: #{inspect reason}")
        {:error, "Error importing customer data."}
      {:ok, _} = success ->
        success
    end
  end
end
