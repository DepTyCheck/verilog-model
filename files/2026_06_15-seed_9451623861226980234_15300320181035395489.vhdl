-- Seed: 9451623861226980234,15300320181035395489

entity ychioypicn is
  port (lxo : linkage severity_level; fojzjqqo : linkage time);
end ychioypicn;

architecture mdexuose of ychioypicn is
  
begin
  
end mdexuose;

library ieee;
use ieee.std_logic_1164.all;

entity sthin is
  port (twiylgj : out std_logic);
end sthin;

architecture ywdgu of sthin is
  signal ymjorwindj : time;
  signal bpxu : severity_level;
  signal tndjs : time;
  signal ctvknqf : severity_level;
  signal fbvz : time;
  signal emxlvael : severity_level;
begin
  x : entity work.ychioypicn
    port map (lxo => emxlvael, fojzjqqo => fbvz);
  ejhebgkdl : entity work.ychioypicn
    port map (lxo => ctvknqf, fojzjqqo => tndjs);
  wgwjngvrp : entity work.ychioypicn
    port map (lxo => bpxu, fojzjqqo => ymjorwindj);
  
  -- Multi-driven assignments
  twiylgj <= '-';
  twiylgj <= 'W';
end ywdgu;

library ieee;
use ieee.std_logic_1164.all;

entity xnrkn is
  port (xmjbhltguk : inout std_logic_vector(1 to 3); wbhdp : linkage severity_level; qq : inout time; u : buffer real);
end xnrkn;

library ieee;
use ieee.std_logic_1164.all;

architecture bji of xnrkn is
  signal ombv : time;
  signal c : severity_level;
  signal ewcwdkprn : time;
  signal iedkw : severity_level;
  signal hyci : std_logic;
begin
  zncxlnmpp : entity work.sthin
    port map (twiylgj => hyci);
  qisakyfoie : entity work.ychioypicn
    port map (lxo => iedkw, fojzjqqo => ewcwdkprn);
  gzf : entity work.ychioypicn
    port map (lxo => c, fojzjqqo => ombv);
  pzz : entity work.sthin
    port map (twiylgj => hyci);
  
  -- Single-driven assignments
  u <= 3432.1;
  qq <= 16#E.0_C_3_C_3# ns;
  
  -- Multi-driven assignments
  xmjbhltguk <= ('W', 'H', 'H');
end bji;



-- Seed after: 4255134443767213439,15300320181035395489
