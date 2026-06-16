-- Seed: 736116054878455769,5472058987609252853

library ieee;
use ieee.std_logic_1164.all;

entity wjwijh is
  port (kjcj : linkage time; ixtw : inout std_logic_vector(1 to 3));
end wjwijh;

architecture xuva of wjwijh is
  
begin
  -- Multi-driven assignments
  ixtw <= ('0', 'Z', 'Z');
end xuva;

entity ocrrh is
  port (enwhq : out time);
end ocrrh;

library ieee;
use ieee.std_logic_1164.all;

architecture ckumhkt of ocrrh is
  signal iuidgk : time;
  signal sdomdauz : std_logic_vector(1 to 3);
  signal tavdgmtdmh : time;
  signal dhlnjszeuc : std_logic_vector(1 to 3);
  signal bbpmnqlar : time;
begin
  pptkeigkh : entity work.wjwijh
    port map (kjcj => bbpmnqlar, ixtw => dhlnjszeuc);
  herfl : entity work.wjwijh
    port map (kjcj => tavdgmtdmh, ixtw => sdomdauz);
  wiyk : entity work.wjwijh
    port map (kjcj => enwhq, ixtw => dhlnjszeuc);
  s : entity work.wjwijh
    port map (kjcj => iuidgk, ixtw => dhlnjszeuc);
  
  -- Multi-driven assignments
  dhlnjszeuc <= ('H', 'L', 'L');
  sdomdauz <= "0X1";
  dhlnjszeuc <= "U1X";
end ckumhkt;

entity gcyurpig is
  port (hkfaxbuf : inout character; fxnotmv : linkage bit; af : linkage bit_vector(2 to 4); bvimh : in character);
end gcyurpig;

library ieee;
use ieee.std_logic_1164.all;

architecture e of gcyurpig is
  signal kdpblvi : std_logic_vector(1 to 3);
  signal zavulh : time;
  signal lm : time;
  signal mxlvxk : std_logic_vector(1 to 3);
  signal g : time;
begin
  cilbhfos : entity work.wjwijh
    port map (kjcj => g, ixtw => mxlvxk);
  wi : entity work.wjwijh
    port map (kjcj => lm, ixtw => mxlvxk);
  chuozv : entity work.wjwijh
    port map (kjcj => zavulh, ixtw => kdpblvi);
  
  -- Multi-driven assignments
  mxlvxk <= "U-W";
  mxlvxk <= "Z1X";
  mxlvxk <= ('W', '-', 'X');
  mxlvxk <= "HH1";
end e;



-- Seed after: 11337391661155884446,5472058987609252853
