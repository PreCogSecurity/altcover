# Development image for building and testing AltCover.
#
# Security notes:
#   * Runs as an unprivileged user, never root.
#   * Relies on .dockerignore to keep .git and the Build/*.snk strong-name
#     private keys out of the image; see SECURITY.md.
#   * Base image is pinned to an explicit Ubuntu LTS release (never :latest) so
#     two builds of the same commit get the same filesystem. Pin the digest as
#     well (`ubuntu:22.04@sha256:...`) once your registry can mirror it.
FROM ubuntu:22.04

LABEL org.opencontainers.image.title="altcover-dev" \
      org.opencontainers.image.description="Development shell for building and testing AltCover" \
      org.opencontainers.image.source="https://github.com/PreCogSecurity/altcover" \
      org.opencontainers.image.licenses="MIT"

# UID/GID of the unprivileged account. docker-compose.yml forwards these from
# .env so that the bind-mounted working tree stays writable on Linux hosts; on
# Windows/Docker Desktop the values are ignored by the filesystem layer.
ARG ALTCOVER_UID=1000
ARG ALTCOVER_GID=1000

# Keep the toolchain quiet and hermetic-ish; see .env.example.
ENV DOTNET_CLI_TELEMETRY_OPTOUT=1 \
    DOTNET_NOLOGO=1 \
    DOTNET_SKIP_FIRST_TIME_EXPERIENCE=1 \
    NUGET_PACKAGES=/app/.nuget/packages \
    HOME=/home/altcover

RUN groupadd -g "$ALTCOVER_GID" altcover \
 && useradd -u "$ALTCOVER_UID" -g "$ALTCOVER_GID" -m -s /bin/bash altcover

WORKDIR /app
COPY --chown=altcover:altcover . .

USER altcover

CMD ["/bin/bash"]
