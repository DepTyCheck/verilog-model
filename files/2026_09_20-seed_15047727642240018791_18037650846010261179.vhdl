-- Seed: 15047727642240018791,18037650846010261179

library ieee;
use ieee.std_logic_1164.all;

entity mwxod is
  port (pgbw : out std_logic_vector(1 downto 1); qnqrs : buffer real; qhrbxdlt : out std_logic_vector(0 downto 2));
end mwxod;

architecture rsuwolo of mwxod is
  
begin
  -- Single-driven assignments
  qnqrs <= 34441.2;
end rsuwolo;

entity gtgufl is
  port (bdd : out time; ojfipxx : linkage time; dulm : in integer; tvypyte : out real);
end gtgufl;

library ieee;
use ieee.std_logic_1164.all;

architecture oedz of gtgufl is
  signal yyy : std_logic_vector(0 downto 2);
  signal w : real;
  signal i : std_logic_vector(1 downto 1);
begin
  sp : entity work.mwxod
    port map (pgbw => i, qnqrs => w, qhrbxdlt => yyy);
  
  -- Single-driven assignments
  tvypyte <= 2#1.11#;
  bdd <= 1_4_4_3_3.232 fs;
  
  -- Multi-driven assignments
  i <= i;
  i <= i;
  i <= i;
end oedz;

entity yicqxw is
  port (lktdnzgbtw : out severity_level; zh : buffer real; vlziy : out boolean_vector(0 downto 3));
end yicqxw;

library ieee;
use ieee.std_logic_1164.all;

architecture wbadk of yicqxw is
  signal ubhsofcek : time;
  signal c : time;
  signal nz : real;
  signal cgt : integer;
  signal pijxex : time;
  signal cfqm : time;
  signal ji : std_logic_vector(0 downto 2);
  signal wkt : real;
  signal ucr : std_logic_vector(1 downto 1);
  signal mjuurvz : std_logic_vector(0 downto 2);
  signal oikptnzj : real;
  signal bfhz : std_logic_vector(1 downto 1);
begin
  kkummvs : entity work.mwxod
    port map (pgbw => bfhz, qnqrs => oikptnzj, qhrbxdlt => mjuurvz);
  ftdhv : entity work.mwxod
    port map (pgbw => ucr, qnqrs => wkt, qhrbxdlt => ji);
  gc : entity work.gtgufl
    port map (bdd => cfqm, ojfipxx => pijxex, dulm => cgt, tvypyte => nz);
  meyggtxwez : entity work.gtgufl
    port map (bdd => c, ojfipxx => ubhsofcek, dulm => cgt, tvypyte => zh);
  
  -- Single-driven assignments
  lktdnzgbtw <= FAILURE;
  cgt <= cgt;
  vlziy <= (others => TRUE);
  
  -- Multi-driven assignments
  bfhz <= (others => 'X');
  bfhz <= bfhz;
  bfhz <= (others => 'U');
  bfhz <= bfhz;
end wbadk;



-- Seed after: 3172028335789808600,18037650846010261179
