-- Seed: 898870090254111619,10940991575366938685

library ieee;
use ieee.std_logic_1164.all;

entity fsxypmwb is
  port (mx : buffer string(3 to 2); neog : out bit_vector(0 to 2); rda : inout std_logic_vector(3 downto 3));
end fsxypmwb;

architecture xnguzexu of fsxypmwb is
  
begin
  -- Single-driven assignments
  mx <= "";
  neog <= ('0', '1', '0');
end xnguzexu;

library ieee;
use ieee.std_logic_1164.all;

entity jlvir is
  port (mrphjo : inout integer; gwzniuk : out real; txzofmttb : buffer std_logic; nd : buffer std_logic);
end jlvir;

library ieee;
use ieee.std_logic_1164.all;

architecture gfy of jlvir is
  signal awizey : bit_vector(0 to 2);
  signal wjjifqkhbd : string(3 to 2);
  signal bujg : std_logic_vector(3 downto 3);
  signal lswksvhrf : bit_vector(0 to 2);
  signal fx : string(3 to 2);
begin
  ykdyrb : entity work.fsxypmwb
    port map (mx => fx, neog => lswksvhrf, rda => bujg);
  exw : entity work.fsxypmwb
    port map (mx => wjjifqkhbd, neog => awizey, rda => bujg);
  
  -- Multi-driven assignments
  bujg <= "U";
  nd <= 'Z';
  nd <= 'Z';
end gfy;

library ieee;
use ieee.std_logic_1164.all;

entity lugohw is
  port (wgsrzwpnul : linkage real; inuvl : linkage integer; jqowblguvu : out std_logic);
end lugohw;

library ieee;
use ieee.std_logic_1164.all;

architecture vvzhjzl of lugohw is
  signal zfksf : std_logic_vector(3 downto 3);
  signal ym : bit_vector(0 to 2);
  signal hakxofowe : string(3 to 2);
  signal rtmsumh : real;
  signal zmme : integer;
  signal wl : std_logic_vector(3 downto 3);
  signal r : bit_vector(0 to 2);
  signal haqf : string(3 to 2);
begin
  hio : entity work.fsxypmwb
    port map (mx => haqf, neog => r, rda => wl);
  gmr : entity work.jlvir
    port map (mrphjo => zmme, gwzniuk => rtmsumh, txzofmttb => jqowblguvu, nd => jqowblguvu);
  myhntfble : entity work.fsxypmwb
    port map (mx => hakxofowe, neog => ym, rda => zfksf);
  
  -- Multi-driven assignments
  jqowblguvu <= 'U';
end vvzhjzl;



-- Seed after: 17193531090173108727,10940991575366938685
