-- Seed: 3863786302487760998,1112937151005418631

library ieee;
use ieee.std_logic_1164.all;

entity dmw is
  port (bbf : in time; srixeqguj : buffer std_logic_vector(0 to 2); eoayopdwbx : in time);
end dmw;

architecture uxsh of dmw is
  
begin
  -- Multi-driven assignments
  srixeqguj <= ('-', '-', 'X');
end uxsh;

library ieee;
use ieee.std_logic_1164.all;

entity zcimmugyq is
  port (tbbn : in real; jsmxix : buffer std_logic_vector(4 to 3); xx : linkage integer; tfk : out real);
end zcimmugyq;

library ieee;
use ieee.std_logic_1164.all;

architecture xhlius of zcimmugyq is
  signal fzxk : time;
  signal av : std_logic_vector(0 to 2);
  signal rjmy : time;
  signal evzh : time;
  signal mciwkzppzn : std_logic_vector(0 to 2);
  signal wz : time;
begin
  cv : entity work.dmw
    port map (bbf => wz, srixeqguj => mciwkzppzn, eoayopdwbx => wz);
  fnhefmctq : entity work.dmw
    port map (bbf => wz, srixeqguj => mciwkzppzn, eoayopdwbx => evzh);
  ovvqf : entity work.dmw
    port map (bbf => rjmy, srixeqguj => av, eoayopdwbx => fzxk);
end xhlius;

library ieee;
use ieee.std_logic_1164.all;

entity qabmjyhsgn is
  port (qwpedtk : out std_logic; pwsfu : out severity_level);
end qabmjyhsgn;

library ieee;
use ieee.std_logic_1164.all;

architecture v of qabmjyhsgn is
  signal yek : real;
  signal uzfdngfsoq : integer;
  signal kfoptknz : std_logic_vector(4 to 3);
  signal kzcrtejdsy : real;
  signal aoy : time;
  signal vq : std_logic_vector(0 to 2);
  signal cbhbm : time;
begin
  fvymycxa : entity work.dmw
    port map (bbf => cbhbm, srixeqguj => vq, eoayopdwbx => aoy);
  nzxwzhhdm : entity work.dmw
    port map (bbf => cbhbm, srixeqguj => vq, eoayopdwbx => cbhbm);
  txns : entity work.zcimmugyq
    port map (tbbn => kzcrtejdsy, jsmxix => kfoptknz, xx => uzfdngfsoq, tfk => yek);
  
  -- Single-driven assignments
  pwsfu <= WARNING;
  cbhbm <= cbhbm;
  
  -- Multi-driven assignments
  qwpedtk <= qwpedtk;
  vq <= vq;
  qwpedtk <= qwpedtk;
end v;



-- Seed after: 16242342812539331014,1112937151005418631
