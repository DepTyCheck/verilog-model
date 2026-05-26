-- Seed: 2174701361152522043,8089241273282434469



entity ydlghsr is
  port (pkhx : out time; gyue : out severity_level; na : out real; yhkf : linkage time);
end ydlghsr;



architecture tchtdqizz of ydlghsr is
  
begin
  
end tchtdqizz;

library ieee;
use ieee.std_logic_1164.all;

entity azwvz is
  port (rbfvxlfi : buffer real; arjhnha : linkage bit_vector(3 downto 3); wxguhoce : buffer time; dlsxck : out std_logic_vector(1 downto 3));
end azwvz;



architecture fjagqnm of azwvz is
  
begin
  
end fjagqnm;



entity uarsok is
  port (ocrjaj : buffer bit; b : linkage time);
end uarsok;

library ieee;
use ieee.std_logic_1164.all;

architecture fbsdiilv of uarsok is
  signal xqztptjjk : std_logic_vector(1 downto 3);
  signal jgabsebkjm : bit_vector(3 downto 3);
  signal catva : real;
  signal klikgnhye : time;
  signal fqtdidrxt : real;
  signal tcejxl : severity_level;
  signal yzkpycvei : time;
  signal xdplwpvuy : real;
  signal st : severity_level;
  signal iiyerlvbey : time;
begin
  v : entity work.ydlghsr
    port map (pkhx => iiyerlvbey, gyue => st, na => xdplwpvuy, yhkf => yzkpycvei);
  axnidk : entity work.ydlghsr
    port map (pkhx => yzkpycvei, gyue => tcejxl, na => fqtdidrxt, yhkf => klikgnhye);
  g : entity work.azwvz
    port map (rbfvxlfi => catva, arjhnha => jgabsebkjm, wxguhoce => klikgnhye, dlsxck => xqztptjjk);
end fbsdiilv;

library ieee;
use ieee.std_logic_1164.all;

entity x is
  port (amzgaevcf : out real; cxlngksom : inout std_logic);
end x;



architecture dr of x is
  signal moourtkvye : time;
  signal si : bit;
  signal j : severity_level;
  signal rik : time;
  signal klfsxhkoi : time;
  signal knwaxu : bit;
begin
  hv : entity work.uarsok
    port map (ocrjaj => knwaxu, b => klfsxhkoi);
  hyawrx : entity work.ydlghsr
    port map (pkhx => rik, gyue => j, na => amzgaevcf, yhkf => klfsxhkoi);
  e : entity work.uarsok
    port map (ocrjaj => si, b => moourtkvye);
end dr;



-- Seed after: 1681752079007966737,8089241273282434469
