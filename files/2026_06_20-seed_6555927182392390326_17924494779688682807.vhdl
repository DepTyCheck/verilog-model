-- Seed: 6555927182392390326,17924494779688682807

entity fwr is
  port (iif : inout severity_level; npdrnty : buffer time; y : inout integer; eq : out real);
end fwr;

architecture u of fwr is
  
begin
  -- Single-driven assignments
  eq <= 1.4;
  iif <= FAILURE;
  npdrnty <= 0 min;
  y <= 2_3_1_3;
end u;

library ieee;
use ieee.std_logic_1164.all;

entity xmemokqf is
  port (iuyieplrjn : linkage bit; jgrlpsnko : in integer; htsuvx : inout std_logic_vector(2 downto 0); ox : inout std_logic_vector(1 downto 1));
end xmemokqf;

architecture k of xmemokqf is
  signal ms : real;
  signal opzbe : integer;
  signal i : time;
  signal xedthtay : severity_level;
begin
  kibjfv : entity work.fwr
    port map (iif => xedthtay, npdrnty => i, y => opzbe, eq => ms);
end k;

library ieee;
use ieee.std_logic_1164.all;

entity dahu is
  port (jdltpjeh : inout integer; f : in std_logic);
end dahu;

library ieee;
use ieee.std_logic_1164.all;

architecture xgpqiq of dahu is
  signal licrpdr : real;
  signal rwxl : integer;
  signal savpd : time;
  signal pfeofcepw : severity_level;
  signal fsguaad : std_logic_vector(1 downto 1);
  signal elkizq : std_logic_vector(2 downto 0);
  signal fhykffnm : bit;
begin
  o : entity work.xmemokqf
    port map (iuyieplrjn => fhykffnm, jgrlpsnko => jdltpjeh, htsuvx => elkizq, ox => fsguaad);
  cz : entity work.fwr
    port map (iif => pfeofcepw, npdrnty => savpd, y => rwxl, eq => licrpdr);
  
  -- Single-driven assignments
  jdltpjeh <= 3;
  
  -- Multi-driven assignments
  elkizq <= ('Z', 'L', '1');
  elkizq <= "-0H";
end xgpqiq;



-- Seed after: 12739454703511341480,17924494779688682807
