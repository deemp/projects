// """
// Creating a Kubernetes Deployment
// """

import * as pulumi from "@pulumi/pulumi"
import * as k8s from "@pulumi/kubernetes"
import { Input } from "@pulumi/pulumi"

function mkFullName(environment: string, name: string): string {
  return `${environment}-${name}`
}

interface PostgresConfig {
  name: string
  configMap: {
    data: {
      POSTGRES_DB: string
      POSTGRES_USER: string
      POSTGRES_PASSWORD: string
    }
  }
  persistentVolume: {
    hostPath: string
  }
  container: {
    image: string
    replicaCount: number
    tag: string
    volumeMounts: {
      name: string
      mountPath: string
    }
  }
  dataBase: {
    port: number
  }
  service: {
    type: Input<"NodePort">
    port: number
    nodePort: number
  }
}

function mkPostgres(
  config: PostgresConfig,
  environment: string,
  provider: k8s.Provider
) {
  // const appName = config.name
  const fullName = mkFullName(environment, config.name)
  const dbConfig = config.dataBase
  const containerConfig = config.container
  const persistentVolumeConfig = config.persistentVolume
  const serviceConfig = config.service
  const labels = { environment: environment }

  const opts = {
    provider: provider,
    // TODO need it?
    deleteBeforeReplace: true,
  }

  const nameSpace = ((name = `${fullName}-namespace`) => new k8s.core.v1.Namespace(name, {
    metadata: { name }
  }, { provider }))()

  // const fullName = `${environment}-${nameSpace.metadata.name}`

  const configMap = ((name = `${fullName}-configmap`) =>
    new k8s.core.v1.ConfigMap(
      name,
      {
        metadata: {
          name: name,
          namespace: nameSpace.metadata.name,
          labels: labels,
        },
        data: config.configMap.data,
      },
      opts
    ))()


  const persistentVolume = ((name = `${fullName}-persistent-volume`) =>
    new k8s.core.v1.PersistentVolume(
      name,
      {
        metadata: {
          name: name,
          namespace: nameSpace.metadata.name,
          labels: labels,
        },
        spec: {
          storageClassName: "manual",
          capacity: {
            storage: "5Gi",
          },
          accessModes: ["ReadWriteMany"],
          hostPath: {
            path: persistentVolumeConfig.hostPath,
          },
        },
      },
      opts
    ))()

  const persistentVolumeClaim = ((
    name = `${fullName}-persistent-volume-claim`
  ) =>
    new k8s.core.v1.PersistentVolumeClaim(
      name,
      {
        metadata: {
          name: name,
          labels: labels,
          namespace: nameSpace.metadata.name,
        },
        spec: {
          storageClassName: "manual",
          accessModes: ["ReadWriteMany"],
          resources: {
            requests: {
              storage: "5Gi",
            },
          },
        },
      },
      opts
    ))()

  const dbPort = "db-port"

  const deployment = ((name = `${fullName}-deployment`) =>
    new k8s.apps.v1.Deployment(
      name,
      {
        metadata: {
          name: name,
          labels: labels,
          namespace: nameSpace.metadata.name,
        },
        spec: {
          replicas: 1,
          selector: {
            matchLabels: labels,
          },
          template: {
            metadata: {
              labels: labels,
            },
            spec: ((volumeName = `${name}-container-volume`) => {
              return {
                containers: [
                  {
                    name: `${name}-container`,
                    image: containerConfig.image,
                    imagePullPolicy: "IfNotPresent",
                    ports: [
                      {
                        containerPort: dbConfig.port,
                        name: dbPort,
                      },
                    ],
                    envFrom: [
                      {
                        configMapRef: {
                          name: configMap.metadata.name,
                        },
                      },
                    ],
                    volumeMounts: [
                      {
                        name: volumeName,
                        mountPath: containerConfig.volumeMounts.mountPath,
                      },
                    ],
                    resources: {
                      requests: {
                        memory: "64Mi",
                        cpu: "250m",
                      },
                      limits: {
                        memory: "128Mi",
                        cpu: "500m",
                      },
                    },
                  },
                ],
                volumes: [
                  {
                    name: volumeName,
                    persistentVolumeClaim: {
                      claimName: persistentVolumeClaim.metadata.name,
                    },
                  },
                ],
              }
            })(),
          },
        },
      },
      opts
    ))()

  const service = ((name = `${fullName}-service`) =>
    new k8s.core.v1.Service(
      name,
      {
        metadata: {
          name: name,
          labels: labels,
          namespace: nameSpace.metadata.name,
        },
        spec: {
          type: serviceConfig.type,
          ports: [
            {
              port: serviceConfig.port,
              targetPort: dbPort,
              nodePort: serviceConfig.nodePort,
            },
          ],
          selector: labels,
        },
      },
      opts
    ))()

  // TODO use just the service name
  const host = service.metadata.name
  const port = serviceConfig.port
  return { host: host, port: port }
}


function mkSetup(
  environment: string,
  provider: k8s.Provider
) {
  const config = new pulumi.Config(environment)

  const postgres = mkPostgres(
    config.requireObject("try-postgresql"),
    environment,
    provider
  )

}

// didn't work with microk8s
// worked with minikube
// const provider = new k8s.Provider("k8s-yaml-rendered", {
//   renderYamlToDirectory: "yaml",
// })
const provider = new k8s.Provider("kubernetes-provider")


if (pulumi.getStack() == "dev") {
  mkSetup("dev", provider)
} else if (pulumi.getStack() == "prod") {
  mkSetup("prod", provider)
}
