-- Seed: 5576804271608127842,8421704836678237495

entity oc is
  port (wdb : in boolean_vector(0 downto 3));
end oc;

architecture mu of oc is
  
begin
  
end mu;

library ieee;
use ieee.std_logic_1164.all;

entity sqhpbfc is
  port (f : in real; brpfzmdc : buffer std_logic; oxoyv : in time; qtkdfoo : inout real_vector(3 downto 0));
end sqhpbfc;

architecture atgth of sqhpbfc is
  signal ebrvqkfg : boolean_vector(0 downto 3);
  signal pumid : boolean_vector(0 downto 3);
begin
  ahzm : entity work.oc
    port map (wdb => pumid);
  c : entity work.oc
    port map (wdb => ebrvqkfg);
  xxevloosl : entity work.oc
    port map (wdb => pumid);
  
  -- Single-driven assignments
  pumid <= (others => TRUE);
  ebrvqkfg <= (others => TRUE);
  qtkdfoo <= (1.0_3_3_1, 3_2_2.4042, 2.0220, 2_0_4_4.4313);
end atgth;

library ieee;
use ieee.std_logic_1164.all;

entity wkueymjsy is
  port (emlo : linkage string(5 to 3); wrdrk : out std_logic_vector(4 downto 3));
end wkueymjsy;

architecture shy of wkueymjsy is
  signal xtn : boolean_vector(0 downto 3);
begin
  smqpxw : entity work.oc
    port map (wdb => xtn);
  
  -- Multi-driven assignments
  wrdrk <= "1X";
  wrdrk <= "X1";
  wrdrk <= "0U";
end shy;

entity ainkqxugt is
  port (sckbmysn : buffer real; ieazgxssfu : buffer real; l : linkage boolean);
end ainkqxugt;

library ieee;
use ieee.std_logic_1164.all;

architecture otdqgmtlu of ainkqxugt is
  signal wscbrftw : string(5 to 3);
  signal mmstnpap : std_logic_vector(4 downto 3);
  signal whonmpvs : string(5 to 3);
begin
  sujckpkxs : entity work.wkueymjsy
    port map (emlo => whonmpvs, wrdrk => mmstnpap);
  xzkpyd : entity work.wkueymjsy
    port map (emlo => wscbrftw, wrdrk => mmstnpap);
  
  -- Single-driven assignments
  ieazgxssfu <= 2#011.00010#;
  sckbmysn <= 16#82.C_2_8_C_1#;
  
  -- Multi-driven assignments
  mmstnpap <= ('L', 'H');
  mmstnpap <= ('Z', 'W');
  mmstnpap <= ('H', 'Z');
end otdqgmtlu;



-- Seed after: 9311443671097816037,8421704836678237495
