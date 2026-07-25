-- Seed: 3745594441932298518,5306691039457971049

entity k is
  port (dnbklsm : linkage time; yxipuj : inout real; islpvurbk : out boolean; y : inout severity_level);
end k;

architecture porqsp of k is
  
begin
  -- Single-driven assignments
  islpvurbk <= TRUE;
  yxipuj <= 3_1_4_3_1.04133;
  y <= NOTE;
end porqsp;

entity zmhrgnrptq is
  port (hdlvnfvtwm : in integer; fjddva : out real);
end zmhrgnrptq;

architecture j of zmhrgnrptq is
  signal cmcczb : severity_level;
  signal utlwpkp : boolean;
  signal lwkfoz : time;
  signal yfl : severity_level;
  signal crb : boolean;
  signal rut : real;
  signal qhv : time;
begin
  swwzu : entity work.k
    port map (dnbklsm => qhv, yxipuj => rut, islpvurbk => crb, y => yfl);
  xefjqwhfw : entity work.k
    port map (dnbklsm => lwkfoz, yxipuj => fjddva, islpvurbk => utlwpkp, y => cmcczb);
end j;

library ieee;
use ieee.std_logic_1164.all;

entity oqew is
  port (tetu : in real; wuh : in std_logic_vector(3 to 3); twdubezp : out integer; tlgiuxwnx : out integer);
end oqew;

architecture ps of oqew is
  signal sptewvydyi : severity_level;
  signal p : boolean;
  signal vqhcuvaa : real;
  signal evvapl : time;
  signal hbzy : severity_level;
  signal umhkbnj : boolean;
  signal ftxahs : real;
  signal winnmqijcs : time;
  signal evvfpp : real;
  signal cdd : integer;
  signal tcevf : real;
begin
  xktugl : entity work.zmhrgnrptq
    port map (hdlvnfvtwm => tlgiuxwnx, fjddva => tcevf);
  f : entity work.zmhrgnrptq
    port map (hdlvnfvtwm => cdd, fjddva => evvfpp);
  ashrptahx : entity work.k
    port map (dnbklsm => winnmqijcs, yxipuj => ftxahs, islpvurbk => umhkbnj, y => hbzy);
  ixcaw : entity work.k
    port map (dnbklsm => evvapl, yxipuj => vqhcuvaa, islpvurbk => p, y => sptewvydyi);
  
  -- Single-driven assignments
  twdubezp <= 8#6#;
  tlgiuxwnx <= 16#0779#;
end ps;

entity ykmfix is
  port (ahnctqf : inout real);
end ykmfix;

library ieee;
use ieee.std_logic_1164.all;

architecture fcamormbd of ykmfix is
  signal v : integer;
  signal var : integer;
  signal stblsshwb : std_logic_vector(3 to 3);
  signal wqid : severity_level;
  signal xps : boolean;
  signal b : real;
  signal m : time;
begin
  covcdlbhc : entity work.k
    port map (dnbklsm => m, yxipuj => b, islpvurbk => xps, y => wqid);
  kfkkzorrw : entity work.oqew
    port map (tetu => ahnctqf, wuh => stblsshwb, twdubezp => var, tlgiuxwnx => v);
  
  -- Single-driven assignments
  ahnctqf <= b;
  
  -- Multi-driven assignments
  stblsshwb <= "1";
  stblsshwb <= (others => 'H');
  stblsshwb <= "H";
  stblsshwb <= stblsshwb;
end fcamormbd;



-- Seed after: 17565687079057150397,5306691039457971049
