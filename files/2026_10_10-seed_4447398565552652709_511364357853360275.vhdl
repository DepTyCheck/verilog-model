-- Seed: 4447398565552652709,511364357853360275

library ieee;
use ieee.std_logic_1164.all;

entity rdpxu is
  port (wxu : out time; nhipmbxf : in time; lctiovexs : inout real; jamkbt : buffer std_logic);
end rdpxu;

architecture ggfdq of rdpxu is
  
begin
  -- Single-driven assignments
  lctiovexs <= 3_2.4_0_4;
  wxu <= 16#2.1_5# ms;
  
  -- Multi-driven assignments
  jamkbt <= '-';
  jamkbt <= jamkbt;
  jamkbt <= jamkbt;
  jamkbt <= jamkbt;
end ggfdq;

entity cejbg is
  port (kmz : in time; udiktsl : buffer time; uxs : out real; xboft : linkage time);
end cejbg;

library ieee;
use ieee.std_logic_1164.all;

architecture yfx of cejbg is
  signal supb : std_logic;
  signal sux : time;
  signal ujfoboi : time;
  signal koe : std_logic;
  signal mcbelmfgy : real;
  signal k : time;
begin
  uqxmjjvpwp : entity work.rdpxu
    port map (wxu => k, nhipmbxf => kmz, lctiovexs => mcbelmfgy, jamkbt => koe);
  fz : entity work.rdpxu
    port map (wxu => ujfoboi, nhipmbxf => sux, lctiovexs => uxs, jamkbt => supb);
  
  -- Single-driven assignments
  udiktsl <= 8#126# us;
  sux <= 2#1_0.0_1_1# ns;
  
  -- Multi-driven assignments
  supb <= 'L';
end yfx;



-- Seed after: 11902035509538084348,511364357853360275
