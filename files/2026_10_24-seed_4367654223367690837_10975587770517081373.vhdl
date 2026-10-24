-- Seed: 4367654223367690837,10975587770517081373

library ieee;
use ieee.std_logic_1164.all;

entity pn is
  port (xlxyrtsy : linkage std_logic; mh : inout std_logic; gj : out std_logic);
end pn;

architecture chokgnvd of pn is
  
begin
  -- Multi-driven assignments
  gj <= mh;
  mh <= 'L';
  gj <= 'Z';
  gj <= 'H';
end chokgnvd;

library ieee;
use ieee.std_logic_1164.all;

entity pz is
  port (ehahnw : buffer std_logic_vector(0 to 3); zjfa : out boolean; twjvqhalva : in real);
end pz;

library ieee;
use ieee.std_logic_1164.all;

architecture gxzvlroyc of pz is
  signal ydrqhypbvt : std_logic;
  signal ee : std_logic;
  signal esbxnzd : std_logic;
  signal gf : std_logic;
  signal rue : std_logic;
  signal ziecxddnp : std_logic;
begin
  edeosn : entity work.pn
    port map (xlxyrtsy => ziecxddnp, mh => rue, gj => ziecxddnp);
  quousdqonj : entity work.pn
    port map (xlxyrtsy => gf, mh => esbxnzd, gj => ziecxddnp);
  ofxrogcmo : entity work.pn
    port map (xlxyrtsy => rue, mh => ee, gj => ydrqhypbvt);
  
  -- Single-driven assignments
  zjfa <= TRUE;
  
  -- Multi-driven assignments
  ehahnw <= ehahnw;
  gf <= 'L';
  ziecxddnp <= ee;
  ehahnw <= ehahnw;
end gxzvlroyc;

library ieee;
use ieee.std_logic_1164.all;

entity eebagft is
  port (i : inout bit_vector(4 to 0); xvhxcnz : inout std_logic_vector(1 downto 3); swrlmmflf : inout std_logic);
end eebagft;

library ieee;
use ieee.std_logic_1164.all;

architecture itdm of eebagft is
  signal bvizm : boolean;
  signal qirx : std_logic;
  signal fjlddfl : std_logic;
  signal qhxj : real;
  signal szedabynhg : boolean;
  signal xwifhox : std_logic_vector(0 to 3);
  signal jplz : std_logic;
  signal tuyiirrcql : std_logic;
begin
  mdovctvi : entity work.pn
    port map (xlxyrtsy => swrlmmflf, mh => tuyiirrcql, gj => jplz);
  cvlrvdiv : entity work.pz
    port map (ehahnw => xwifhox, zjfa => szedabynhg, twjvqhalva => qhxj);
  lqxi : entity work.pn
    port map (xlxyrtsy => fjlddfl, mh => swrlmmflf, gj => qirx);
  raudyttvrw : entity work.pz
    port map (ehahnw => xwifhox, zjfa => bvizm, twjvqhalva => qhxj);
  
  -- Single-driven assignments
  i <= i;
  
  -- Multi-driven assignments
  swrlmmflf <= 'X';
  swrlmmflf <= 'W';
end itdm;



-- Seed after: 10934298804351814075,10975587770517081373
