-- Seed: 12076343440723541890,8118127366649987907

library ieee;
use ieee.std_logic_1164.all;

entity q is
  port ( nx : linkage bit_vector(4 to 3)
  ; khwxa : in time_vector(1 downto 2)
  ; odp : inout std_logic_vector(1 to 4)
  ; dtzmrijjok : linkage integer_vector(2 downto 4)
  );
end q;

architecture jgveqz of q is
  
begin
  
end jgveqz;

entity ncroued is
  port (svun : inout time);
end ncroued;

library ieee;
use ieee.std_logic_1164.all;

architecture xg of ncroued is
  signal txgnhuqmao : integer_vector(2 downto 4);
  signal y : std_logic_vector(1 to 4);
  signal aqbd : time_vector(1 downto 2);
  signal gwckqrppk : bit_vector(4 to 3);
  signal dfvwnxny : integer_vector(2 downto 4);
  signal slauutfmzb : std_logic_vector(1 to 4);
  signal smxn : bit_vector(4 to 3);
  signal ihqqccjz : integer_vector(2 downto 4);
  signal zzicokyuis : time_vector(1 downto 2);
  signal pibcmekp : bit_vector(4 to 3);
  signal u : integer_vector(2 downto 4);
  signal i : std_logic_vector(1 to 4);
  signal dwmhvsafgb : time_vector(1 downto 2);
  signal pppwdnq : bit_vector(4 to 3);
begin
  gjeqvoho : entity work.q
    port map (nx => pppwdnq, khwxa => dwmhvsafgb, odp => i, dtzmrijjok => u);
  ekculk : entity work.q
    port map (nx => pibcmekp, khwxa => zzicokyuis, odp => i, dtzmrijjok => ihqqccjz);
  mzht : entity work.q
    port map (nx => smxn, khwxa => dwmhvsafgb, odp => slauutfmzb, dtzmrijjok => dfvwnxny);
  ocbcnma : entity work.q
    port map (nx => gwckqrppk, khwxa => aqbd, odp => y, dtzmrijjok => txgnhuqmao);
  
  -- Multi-driven assignments
  i <= "-000";
  slauutfmzb <= "Z0-1";
end xg;



-- Seed after: 14219730394536541759,8118127366649987907
