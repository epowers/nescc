Summary: galsC compiler 
Name: galsc
Version: 0.1
Release: 0
License: GNU GPL Version 2
Packager: Palo Alto Research Center
Group: Development/Tools
URL: http://sourceforge.net/projects/galsc
Source0: %{name}-%{version}.tar.gz

%description 
galsC is a compiler for a new language (based on nesC, which is based
on C) to support the TinyGALS (Globally Asynchronous, Locally
Synchronous) programming model. This compiler is designed to work with
the TinyOS project.

%prep
%setup -q

%build
./configure TOSDIR=/usr/local/src/tinyos-1.x/tos/
make 

%install
make install

%clean
rm -rf $RPM_BUILD_DIR/%{name}-%{version}

%files
/usr/local/lib/ncc/
/usr/local/bin/mig
/usr/local/bin/ncg
/usr/local/bin/ncc
/usr/local/bin/nescc
/usr/local/bin/nesdoc

%defattr(-,root,root,-)
%doc

%changelog

