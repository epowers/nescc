define pname
  if $arg0->container
    printf "%s$", $arg0->container->name
  end
  if $arg0->interface
    printf "%s$", $arg0->interface->name
  end
  printf "%s\n", $arg0->name
end

define pendp
  if $arg0->actor
    printf "%s", $arg0->actor->name
  end
  printf "."

  if $arg0->port
    printf "%s", $arg0->port->name
  end
  printf "."

  if $arg0->component
    printf "%s", $arg0->component->name
  end
  printf "."

  if $arg0->interface
    printf "%s", $arg0->interface->name
  end
  printf "."

  if $arg0->function
    printf "%s", $arg0->function->name
  end

  printf "\n"

end
