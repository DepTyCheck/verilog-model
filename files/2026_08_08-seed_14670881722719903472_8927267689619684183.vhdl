-- Seed: 14670881722719903472,8927267689619684183

library ieee;
use ieee.std_logic_1164.all;

entity km is
  port (julpwjkp : in integer; apiuacmkz : out std_logic_vector(2 downto 3); pexjzho : buffer std_logic; ie : in time);
end km;

architecture lb of km is
  
begin
  -- Multi-driven assignments
  apiuacmkz <= apiuacmkz;
  pexjzho <= pexjzho;
end lb;

entity xfxuberdmb is
  port (g : inout time; idvuu : out severity_level);
end xfxuberdmb;

library ieee;
use ieee.std_logic_1164.all;

architecture pmfzsucrg of xfxuberdmb is
  signal oc : std_logic;
  signal lkifc : std_logic_vector(2 downto 3);
  signal uhzsekx : integer;
begin
  bfpmg : entity work.km
    port map (julpwjkp => uhzsekx, apiuacmkz => lkifc, pexjzho => oc, ie => g);
  
  -- Single-driven assignments
  uhzsekx <= 8#1_0#;
end pmfzsucrg;

library ieee;
use ieee.std_logic_1164.all;

entity njkhzkn is
  port (nblyskivb : out std_logic; djyfkujl : inout real);
end njkhzkn;

architecture athlsipnsq of njkhzkn is
  
begin
  -- Single-driven assignments
  djyfkujl <= 8#644.4_2#;
  
  -- Multi-driven assignments
  nblyskivb <= nblyskivb;
end athlsipnsq;

entity wfgz is
  port (cut : buffer severity_level; gjvinho : linkage bit; wcweleboh : inout time; xk : inout boolean);
end wfgz;

library ieee;
use ieee.std_logic_1164.all;

architecture w of wfgz is
  signal h : severity_level;
  signal lde : std_logic;
  signal umhoctxpem : std_logic_vector(2 downto 3);
  signal bwwgb : std_logic;
  signal sdv : std_logic_vector(2 downto 3);
  signal ho : time;
  signal umucpq : std_logic;
  signal rc : std_logic_vector(2 downto 3);
  signal xrvyk : integer;
begin
  woygdxxg : entity work.km
    port map (julpwjkp => xrvyk, apiuacmkz => rc, pexjzho => umucpq, ie => ho);
  rudl : entity work.km
    port map (julpwjkp => xrvyk, apiuacmkz => sdv, pexjzho => bwwgb, ie => wcweleboh);
  qcnc : entity work.km
    port map (julpwjkp => xrvyk, apiuacmkz => umhoctxpem, pexjzho => lde, ie => wcweleboh);
  yteuiudn : entity work.xfxuberdmb
    port map (g => wcweleboh, idvuu => h);
  
  -- Single-driven assignments
  xk <= xk;
  cut <= WARNING;
  
  -- Multi-driven assignments
  umucpq <= bwwgb;
end w;



-- Seed after: 15273418697192030578,8927267689619684183
