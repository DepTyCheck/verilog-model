-- Seed: 6618473281803060823,14641901754878719179

library ieee;
use ieee.std_logic_1164.all;

entity pwmeb is
  port (pvaptsxjkf : in std_logic_vector(1 downto 2); iklbtgck : in string(1 to 3); t : linkage time; uy : inout real);
end pwmeb;

architecture xisvlsdo of pwmeb is
  
begin
  
end xisvlsdo;

entity fmh is
  port (wsvm : inout boolean; xplkyky : buffer integer; yetsziarrx : linkage integer);
end fmh;

library ieee;
use ieee.std_logic_1164.all;

architecture vm of fmh is
  signal sbskvw : real;
  signal jd : time;
  signal iwqbyf : string(1 to 3);
  signal f : real;
  signal nmgebmljbb : time;
  signal zp : std_logic_vector(1 downto 2);
  signal aaw : real;
  signal cpi : time;
  signal z : string(1 to 3);
  signal rtbpnw : std_logic_vector(1 downto 2);
begin
  wur : entity work.pwmeb
    port map (pvaptsxjkf => rtbpnw, iklbtgck => z, t => cpi, uy => aaw);
  yoy : entity work.pwmeb
    port map (pvaptsxjkf => zp, iklbtgck => z, t => nmgebmljbb, uy => f);
  n : entity work.pwmeb
    port map (pvaptsxjkf => rtbpnw, iklbtgck => iwqbyf, t => jd, uy => sbskvw);
  
  -- Single-driven assignments
  xplkyky <= 2#11110#;
  
  -- Multi-driven assignments
  zp <= "";
  rtbpnw <= rtbpnw;
  zp <= rtbpnw;
  rtbpnw <= rtbpnw;
end vm;



-- Seed after: 8442663726932556132,14641901754878719179
