Air.Repo.Seeder.seed()

:ok = Air.Service.License.load(File.read!("priv/dev_license.lic"))
