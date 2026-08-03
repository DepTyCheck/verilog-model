-- Seed: 15981280049160676702,12359743974512393525

library ieee;
use ieee.std_logic_1164.all;

entity hmfk is
  port (zdasryg : in std_logic_vector(4 to 3); kevebs : linkage real; j : buffer time_vector(4 to 1));
end hmfk;

architecture zjjf of hmfk is
  
begin
  
end zjjf;

library ieee;
use ieee.std_logic_1164.all;

entity hsvycc is
  port (mdgd : out std_logic_vector(4 to 1); gkklrx : buffer boolean; y : in time);
end hsvycc;

library ieee;
use ieee.std_logic_1164.all;

architecture ti of hsvycc is
  signal jqrk : time_vector(4 to 1);
  signal ukaowa : real;
  signal wn : std_logic_vector(4 to 3);
  signal enrnl : time_vector(4 to 1);
  signal kwacq : real;
  signal szcteiu : std_logic_vector(4 to 3);
  signal auhgmhsj : time_vector(4 to 1);
  signal w : real;
  signal rwkmeznyti : std_logic_vector(4 to 3);
  signal lruw : time_vector(4 to 1);
  signal a : real;
begin
  fnavcrsn : entity work.hmfk
    port map (zdasryg => mdgd, kevebs => a, j => lruw);
  i : entity work.hmfk
    port map (zdasryg => rwkmeznyti, kevebs => w, j => auhgmhsj);
  muio : entity work.hmfk
    port map (zdasryg => szcteiu, kevebs => kwacq, j => enrnl);
  f : entity work.hmfk
    port map (zdasryg => wn, kevebs => ukaowa, j => jqrk);
  
  -- Single-driven assignments
  gkklrx <= TRUE;
  
  -- Multi-driven assignments
  mdgd <= "";
  mdgd <= mdgd;
end ti;



-- Seed after: 3190893356664306003,12359743974512393525
