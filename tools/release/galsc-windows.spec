Summary: galsC compiler 
Name: galsc
Version: 0.1.0
Release: 0w
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
./configure --prefix=/usr/local/galsc TOSDIR=/usr/local/src/tinyos-1.x/tos/
make 

%install
make install

%clean
rm -rf $RPM_BUILD_DIR/%{name}-%{version}

%files
/usr/local/galsc/lib/ncc/
/usr/local/galsc/bin/galscc
/usr/local/galsc/bin/mig
/usr/local/galsc/bin/ncc
/usr/local/galsc/bin/ncg
/usr/local/galsc/bin/nescc
/usr/local/galsc/bin/nesdoc

%defattr(-,root,root,-)
%doc

%changelog
