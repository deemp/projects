set -a

export KUBECONFIG_DIR="$PWD/.kube"
export KUBECONFIG="$KUBECONFIG_DIR/config"
mkdir -p $KUBECONFIG_DIR
microk8s config > $KUBECONFIG

# use commands from microk8s
alias k=microk8s.kubectl
alias kubectl=microk8s.kubectl
alias h=microk8s.helm3
alias helm=microk8s.helm3
source <(k completion bash | sed 's/kubectl/k/g')
source <(kubectl completion bash)
source <(h completion bash | sed 's/helm/h/g')
source <(helm completion bash)
