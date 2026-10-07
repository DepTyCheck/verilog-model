-- Seed: 17210987521132722217,5906004015519833893

library ieee;
use ieee.std_logic_1164.all;

entity y is
  port (nc : in bit_vector(3 to 3); ua : out std_logic);
end y;

architecture gkt of y is
  
begin
  -- Multi-driven assignments
  ua <= 'X';
end gkt;

entity ckhnaquoy is
  port (z : in time_vector(3 to 3); tcs : in time_vector(1 to 4); xfdq : in time);
end ckhnaquoy;

library ieee;
use ieee.std_logic_1164.all;

architecture dksubpljxz of ckhnaquoy is
  signal nqkgztw : bit_vector(3 to 3);
  signal hcdh : bit_vector(3 to 3);
  signal s : std_logic;
  signal pqim : bit_vector(3 to 3);
begin
  mbp : entity work.y
    port map (nc => pqim, ua => s);
  tby : entity work.y
    port map (nc => hcdh, ua => s);
  ci : entity work.y
    port map (nc => nqkgztw, ua => s);
  o : entity work.y
    port map (nc => nqkgztw, ua => s);
  
  -- Multi-driven assignments
  s <= s;
  s <= s;
end dksubpljxz;

library ieee;
use ieee.std_logic_1164.all;

entity egjykqa is
  port (znyxhb : linkage std_logic);
end egjykqa;

library ieee;
use ieee.std_logic_1164.all;

architecture ezbcbs of egjykqa is
  signal ipnueyfjad : time;
  signal dsbjthk : time_vector(1 to 4);
  signal a : time_vector(3 to 3);
  signal xhwxrusudw : std_logic;
  signal ahpyrnce : bit_vector(3 to 3);
  signal yodhkuht : std_logic;
  signal iijppzji : bit_vector(3 to 3);
begin
  at : entity work.y
    port map (nc => iijppzji, ua => yodhkuht);
  owmpzunbcs : entity work.y
    port map (nc => ahpyrnce, ua => xhwxrusudw);
  mxak : entity work.ckhnaquoy
    port map (z => a, tcs => dsbjthk, xfdq => ipnueyfjad);
  
  -- Single-driven assignments
  iijppzji <= (others => '1');
end ezbcbs;



-- Seed after: 17813405508216126262,5906004015519833893
