-- Seed: 7003553101861374039,18037650846010261179

library ieee;
use ieee.std_logic_1164.all;

entity yilkhoze is
  port (d : buffer std_logic; ekauegju : in std_logic_vector(4 downto 3); jf : in integer);
end yilkhoze;

architecture tqdxwo of yilkhoze is
  
begin
  -- Multi-driven assignments
  d <= d;
  d <= '-';
  d <= 'X';
end tqdxwo;

library ieee;
use ieee.std_logic_1164.all;

entity ssab is
  port (fxj : linkage std_logic; srpix : linkage integer_vector(4 downto 0); pzk : buffer real);
end ssab;

library ieee;
use ieee.std_logic_1164.all;

architecture iwsppi of ssab is
  signal mav : std_logic_vector(4 downto 3);
  signal knvxwsq : std_logic;
  signal zctfn : std_logic_vector(4 downto 3);
  signal eydbm : integer;
  signal lxirdjtot : std_logic_vector(4 downto 3);
  signal nibffpclf : std_logic;
begin
  jiv : entity work.yilkhoze
    port map (d => nibffpclf, ekauegju => lxirdjtot, jf => eydbm);
  fsnubezyyw : entity work.yilkhoze
    port map (d => nibffpclf, ekauegju => zctfn, jf => eydbm);
  x : entity work.yilkhoze
    port map (d => knvxwsq, ekauegju => mav, jf => eydbm);
  awolgptyj : entity work.yilkhoze
    port map (d => nibffpclf, ekauegju => lxirdjtot, jf => eydbm);
  
  -- Single-driven assignments
  pzk <= 2#011.0#;
  eydbm <= eydbm;
  
  -- Multi-driven assignments
  zctfn <= lxirdjtot;
end iwsppi;



-- Seed after: 12541060173815647667,18037650846010261179
