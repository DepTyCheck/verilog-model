-- Seed: 1012286569621207353,16159265764638711791

library ieee;
use ieee.std_logic_1164.all;

entity cx is
  port (txctxla : inout std_logic);
end cx;

architecture y of cx is
  
begin
  -- Multi-driven assignments
  txctxla <= 'U';
  txctxla <= txctxla;
end y;

entity jd is
  port (r : inout real; s : buffer integer_vector(1 downto 1); ltmi : in time_vector(2 downto 0); yjfrpx : inout time);
end jd;

library ieee;
use ieee.std_logic_1164.all;

architecture tkhidba of jd is
  signal z : std_logic;
begin
  thmegjfjb : entity work.cx
    port map (txctxla => z);
  gptbtl : entity work.cx
    port map (txctxla => z);
  rvxykk : entity work.cx
    port map (txctxla => z);
  jr : entity work.cx
    port map (txctxla => z);
  
  -- Multi-driven assignments
  z <= z;
end tkhidba;



-- Seed after: 4109248721131140472,16159265764638711791
