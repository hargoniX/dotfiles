run_id=$(python -c "import requests;r=requests.get('https://api.github.com/repos/hargonix/st/actions/workflows/c.yml/runs');print(r.json()['workflow_runs'][0]['id'])")
artifact_url=$(python -c "import requests;r=requests.get('https://api.github.com/repos/hargonix/st/actions/runs/$run_id/artifacts');print(r.json()['artifacts'][0]['archive_download_url'])")
curl -L -u hargonix:$github_token -o /tmp/st.zip $artifact_url
unzip -o /tmp/st.zip -d ~/.local/bin
chmod +x ~/.local/bin/st
