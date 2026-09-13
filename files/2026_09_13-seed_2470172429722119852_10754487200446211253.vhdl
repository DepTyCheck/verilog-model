-- Seed: 2470172429722119852,10754487200446211253

library ieee;
use ieee.std_logic_1164.all;

entity qsi is
  port (smgzrg : out real; yddccixb : in std_logic_vector(0 to 0); dqlounstk : out std_logic);
end qsi;

architecture tpnlcz of qsi is
  
begin
  -- Single-driven assignments
  smgzrg <= 21020.4_4;
  
  -- Multi-driven assignments
  dqlounstk <= '0';
end tpnlcz;

library ieee;
use ieee.std_logic_1164.all;

entity dakprxjt is
  port (lo : buffer std_logic; ngxoceef : in time; vd : in time; imw : linkage time);
end dakprxjt;

library ieee;
use ieee.std_logic_1164.all;

architecture rgxvxrzpz of dakprxjt is
  signal wpi : std_logic;
  signal gojvhdbls : real;
  signal nrmesipk : std_logic;
  signal qaoyhzbd : std_logic_vector(0 to 0);
  signal mksplrosm : real;
  signal lyb : std_logic_vector(0 to 0);
  signal rewpgqleh : real;
begin
  s : entity work.qsi
    port map (smgzrg => rewpgqleh, yddccixb => lyb, dqlounstk => lo);
  lthpkh : entity work.qsi
    port map (smgzrg => mksplrosm, yddccixb => qaoyhzbd, dqlounstk => nrmesipk);
  jbcul : entity work.qsi
    port map (smgzrg => gojvhdbls, yddccixb => lyb, dqlounstk => wpi);
  
  -- Multi-driven assignments
  lo <= 'X';
  qaoyhzbd <= (others => 'X');
  lyb <= "L";
  lyb <= qaoyhzbd;
end rgxvxrzpz;

entity rnmqqzl is
  port (od : linkage real; hcudshchbm : buffer real);
end rnmqqzl;

library ieee;
use ieee.std_logic_1164.all;

architecture eiferw of rnmqqzl is
  signal kdxwor : time;
  signal ctjckb : time;
  signal mlfgvuolp : time;
  signal ytcbbcp : std_logic;
  signal jbdqn : std_logic;
  signal nzj : std_logic_vector(0 to 0);
begin
  omlsnu : entity work.qsi
    port map (smgzrg => hcudshchbm, yddccixb => nzj, dqlounstk => jbdqn);
  imomrboctf : entity work.dakprxjt
    port map (lo => ytcbbcp, ngxoceef => mlfgvuolp, vd => ctjckb, imw => kdxwor);
end eiferw;



-- Seed after: 9942903058071381876,10754487200446211253
