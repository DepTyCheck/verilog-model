-- Seed: 8322332144929331948,13694093582652240945

library ieee;
use ieee.std_logic_1164.all;

entity mpjdsouah is
  port (jtezlhwb : buffer bit_vector(0 to 3); evstjp : in time; vuenqznhuf : in std_logic);
end mpjdsouah;

architecture knojtxolo of mpjdsouah is
  
begin
  -- Single-driven assignments
  jtezlhwb <= ('1', '1', '0', '1');
end knojtxolo;

entity glci is
  port (hknnlrz : buffer time_vector(1 to 3));
end glci;

architecture mp of glci is
  
begin
  -- Single-driven assignments
  hknnlrz <= (4_2_1_3_1 ms, 3_3_0 ns, 0 sec);
end mp;

library ieee;
use ieee.std_logic_1164.all;

entity psvy is
  port (dumt : out std_logic_vector(4 downto 1); vtu : in severity_level);
end psvy;

library ieee;
use ieee.std_logic_1164.all;

architecture g of psvy is
  signal nfmptfzbto : bit_vector(0 to 3);
  signal mog : std_logic;
  signal ig : time;
  signal mkbyqfgodo : bit_vector(0 to 3);
  signal eai : std_logic;
  signal idtzk : bit_vector(0 to 3);
  signal db : std_logic;
  signal dxnxznsrz : time;
  signal echb : bit_vector(0 to 3);
begin
  b : entity work.mpjdsouah
    port map (jtezlhwb => echb, evstjp => dxnxznsrz, vuenqznhuf => db);
  rkbuno : entity work.mpjdsouah
    port map (jtezlhwb => idtzk, evstjp => dxnxznsrz, vuenqznhuf => eai);
  s : entity work.mpjdsouah
    port map (jtezlhwb => mkbyqfgodo, evstjp => ig, vuenqznhuf => mog);
  lsrtk : entity work.mpjdsouah
    port map (jtezlhwb => nfmptfzbto, evstjp => dxnxznsrz, vuenqznhuf => db);
  
  -- Multi-driven assignments
  dumt <= ('W', '1', 'L', 'L');
end g;

library ieee;
use ieee.std_logic_1164.all;

entity yrvwwljb is
  port (jdpuj : in bit_vector(2 downto 2); hcju : linkage time; wcepyhxed : in std_logic);
end yrvwwljb;

library ieee;
use ieee.std_logic_1164.all;

architecture yfr of yrvwwljb is
  signal hknshakoz : std_logic;
  signal b : time;
  signal nghfvezshl : bit_vector(0 to 3);
  signal eortbaxt : std_logic;
  signal mfrful : time;
  signal uzhxe : bit_vector(0 to 3);
  signal xfjpisid : time_vector(1 to 3);
  signal ixs : time_vector(1 to 3);
begin
  crh : entity work.glci
    port map (hknnlrz => ixs);
  lhdipzicnc : entity work.glci
    port map (hknnlrz => xfjpisid);
  ql : entity work.mpjdsouah
    port map (jtezlhwb => uzhxe, evstjp => mfrful, vuenqznhuf => eortbaxt);
  pzw : entity work.mpjdsouah
    port map (jtezlhwb => nghfvezshl, evstjp => b, vuenqznhuf => hknshakoz);
  
  -- Single-driven assignments
  mfrful <= 2#1.1# us;
  b <= 8#077.4_5_6_3# ms;
  
  -- Multi-driven assignments
  eortbaxt <= 'L';
  hknshakoz <= 'W';
  eortbaxt <= 'X';
end yfr;



-- Seed after: 3687480680168215491,13694093582652240945
