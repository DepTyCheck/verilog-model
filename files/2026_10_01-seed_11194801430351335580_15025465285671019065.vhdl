-- Seed: 11194801430351335580,15025465285671019065

library ieee;
use ieee.std_logic_1164.all;

entity rjfu is
  port (whuwkjsvx : out real; ipt : buffer bit_vector(2 downto 1); luhk : buffer integer; wtwwzhv : inout std_logic);
end rjfu;

architecture mxwqa of rjfu is
  
begin
  -- Multi-driven assignments
  wtwwzhv <= 'H';
  wtwwzhv <= wtwwzhv;
end mxwqa;

entity tfsvdtq is
  port (veov : in time);
end tfsvdtq;

library ieee;
use ieee.std_logic_1164.all;

architecture g of tfsvdtq is
  signal lypr : integer;
  signal vlkuzolwiw : bit_vector(2 downto 1);
  signal sjvw : real;
  signal dvrnhx : std_logic;
  signal gsaee : integer;
  signal iwtxwoe : bit_vector(2 downto 1);
  signal bt : real;
begin
  ybvojb : entity work.rjfu
    port map (whuwkjsvx => bt, ipt => iwtxwoe, luhk => gsaee, wtwwzhv => dvrnhx);
  tkuvprrpqp : entity work.rjfu
    port map (whuwkjsvx => sjvw, ipt => vlkuzolwiw, luhk => lypr, wtwwzhv => dvrnhx);
end g;

entity ki is
  port (zqahaheaam : inout bit);
end ki;

library ieee;
use ieee.std_logic_1164.all;

architecture skfhcve of ki is
  signal wf : std_logic;
  signal tgooygtd : integer;
  signal uuaasm : bit_vector(2 downto 1);
  signal vsr : real;
  signal cpvcpc : std_logic;
  signal phd : integer;
  signal mejhy : bit_vector(2 downto 1);
  signal spemc : real;
  signal ffz : time;
begin
  xpggvyl : entity work.tfsvdtq
    port map (veov => ffz);
  zwmqag : entity work.rjfu
    port map (whuwkjsvx => spemc, ipt => mejhy, luhk => phd, wtwwzhv => cpvcpc);
  kiowuumt : entity work.tfsvdtq
    port map (veov => ffz);
  mtbt : entity work.rjfu
    port map (whuwkjsvx => vsr, ipt => uuaasm, luhk => tgooygtd, wtwwzhv => wf);
  
  -- Multi-driven assignments
  cpvcpc <= cpvcpc;
  cpvcpc <= cpvcpc;
end skfhcve;



-- Seed after: 17217110761697287843,15025465285671019065
