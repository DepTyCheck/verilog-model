-- Seed: 17100824736134588991,4860866131898729603

library ieee;
use ieee.std_logic_1164.all;

entity aetibmjlkn is
  port (li : inout std_logic);
end aetibmjlkn;

architecture n of aetibmjlkn is
  
begin
  -- Multi-driven assignments
  li <= '-';
end n;

library ieee;
use ieee.std_logic_1164.all;

entity au is
  port (r : linkage std_logic; gqrqwhgxk : buffer integer);
end au;

library ieee;
use ieee.std_logic_1164.all;

architecture dxolfrs of au is
  signal yh : std_logic;
  signal d : std_logic;
begin
  h : entity work.aetibmjlkn
    port map (li => d);
  w : entity work.aetibmjlkn
    port map (li => yh);
  
  -- Multi-driven assignments
  d <= 'X';
  d <= 'W';
end dxolfrs;

entity fnxplmqgr is
  port (vlpfye : linkage real_vector(0 to 1); ztmny : in real_vector(3 to 2));
end fnxplmqgr;

library ieee;
use ieee.std_logic_1164.all;

architecture hspjkl of fnxplmqgr is
  signal vopgaxr : std_logic;
  signal ibfkhhpz : integer;
  signal vgywxoi : integer;
  signal d : std_logic;
begin
  mrts : entity work.au
    port map (r => d, gqrqwhgxk => vgywxoi);
  zfkeopmyns : entity work.au
    port map (r => d, gqrqwhgxk => ibfkhhpz);
  fzuax : entity work.aetibmjlkn
    port map (li => d);
  mcpzrmizv : entity work.aetibmjlkn
    port map (li => vopgaxr);
end hspjkl;

entity jpfguqqbn is
  port (h : inout integer_vector(1 to 1));
end jpfguqqbn;

architecture ppxfxci of jpfguqqbn is
  signal sdkozrufhi : real_vector(3 to 2);
  signal akgyk : real_vector(0 to 1);
begin
  m : entity work.fnxplmqgr
    port map (vlpfye => akgyk, ztmny => sdkozrufhi);
  
  -- Single-driven assignments
  h <= (others => 16#126#);
  sdkozrufhi <= (others => 0.0);
end ppxfxci;



-- Seed after: 2676220372846287024,4860866131898729603
