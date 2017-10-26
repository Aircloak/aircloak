defmodule CentralWeb.ImportController do
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
    case Central.Service.Customer.import_customer_data(export_data) do
      {:error, reason} ->
        Logger.error("Error importing customer data: #{inspect reason}")
        {:error, import_error(reason)}
      {:ok, _} = success ->
        success
    end
  end

  defp import_error(:invalid_format), do: "Invalid file format!"
  defp import_error(:invalid_token), do: "Invalid customer token!"
  defp import_error(:already_imported), do: "Already imported!"
  defp import_error(:invalid_version), do: "Invalid version specification!"
  defp import_error({:missing_previous_export, nil}), do: "Missing all previous exports for this customer!"
  defp import_error({:missing_previous_export, since}), do:
    "Missing previous exports for this customer! " <>
    "The last known export was generated on " <>
    Timex.format!(since, "{YYYY}/{0M}/{0D} {0h24}:{0m}") <> "."
end
