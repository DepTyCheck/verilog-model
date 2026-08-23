-- Seed: 2407729018838681054,4245627776430562977

library ieee;
use ieee.std_logic_1164.all;

entity ggieeqje is
  port (oxxq : inout integer; ucfo : linkage time; ymqv : inout bit; l : inout std_logic);
end ggieeqje;

architecture ccjtpbvsrh of ggieeqje is
  
begin
  -- Single-driven assignments
  ymqv <= ymqv;
  oxxq <= oxxq;
  
  -- Multi-driven assignments
  l <= l;
  l <= l;
  l <= l;
end ccjtpbvsrh;

library ieee;
use ieee.std_logic_1164.all;

entity ryynwg is
  port ( pmjgvhvlg : out std_logic_vector(2 downto 4)
  ; tfoanf : buffer std_logic_vector(0 downto 0)
  ; xhovaoqhcf : in boolean_vector(3 to 0)
  ; reghgm : out integer
  );
end ryynwg;

architecture nrzih of ryynwg is
  
begin
  -- Single-driven assignments
  reghgm <= 2#0_1_0_1_0#;
end nrzih;

entity zwjfy is
  port (rmnknsq : inout time; gvgvlo : in time);
end zwjfy;

library ieee;
use ieee.std_logic_1164.all;

architecture ssl of zwjfy is
  signal lw : integer;
  signal nxcalhu : boolean_vector(3 to 0);
  signal nnmb : std_logic_vector(0 downto 0);
  signal mo : integer;
  signal sfazydebql : boolean_vector(3 to 0);
  signal yqw : std_logic_vector(0 downto 0);
  signal eqzesd : std_logic_vector(2 downto 4);
  signal gqohoyhusz : std_logic;
  signal tigpmhs : bit;
  signal qedmtiyp : time;
  signal ljbpknzgg : integer;
  signal mzneymo : std_logic;
  signal ocpw : bit;
  signal moz : integer;
begin
  xtlqtl : entity work.ggieeqje
    port map (oxxq => moz, ucfo => rmnknsq, ymqv => ocpw, l => mzneymo);
  vgvdaqaq : entity work.ggieeqje
    port map (oxxq => ljbpknzgg, ucfo => qedmtiyp, ymqv => tigpmhs, l => gqohoyhusz);
  r : entity work.ryynwg
    port map (pmjgvhvlg => eqzesd, tfoanf => yqw, xhovaoqhcf => sfazydebql, reghgm => mo);
  zwwrj : entity work.ryynwg
    port map (pmjgvhvlg => eqzesd, tfoanf => nnmb, xhovaoqhcf => nxcalhu, reghgm => lw);
  
  -- Single-driven assignments
  sfazydebql <= (others => TRUE);
  nxcalhu <= sfazydebql;
  
  -- Multi-driven assignments
  mzneymo <= 'Z';
  eqzesd <= (others => '0');
  mzneymo <= mzneymo;
end ssl;

library ieee;
use ieee.std_logic_1164.all;

entity nqa is
  port (xpri : buffer std_logic; vxihlyhe : linkage time; varcrw : inout string(3 downto 3));
end nqa;

architecture uxpgbd of nqa is
  signal xcuetj : time;
  signal mbvbzxg : time;
  signal zw : time;
  signal xlu : time;
  signal b : time;
  signal rlx : time;
  signal lxvwb : bit;
  signal x : integer;
begin
  atryt : entity work.ggieeqje
    port map (oxxq => x, ucfo => vxihlyhe, ymqv => lxvwb, l => xpri);
  wgvjimmazw : entity work.zwjfy
    port map (rmnknsq => rlx, gvgvlo => b);
  awdpu : entity work.zwjfy
    port map (rmnknsq => xlu, gvgvlo => zw);
  pqnci : entity work.zwjfy
    port map (rmnknsq => mbvbzxg, gvgvlo => xcuetj);
  
  -- Single-driven assignments
  varcrw <= varcrw;
  xcuetj <= 2#0# fs;
  zw <= rlx;
  b <= 4_3_0 ns;
  
  -- Multi-driven assignments
  xpri <= 'W';
end uxpgbd;



-- Seed after: 14267422433722139111,4245627776430562977
