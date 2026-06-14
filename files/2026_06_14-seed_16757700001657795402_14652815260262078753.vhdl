-- Seed: 16757700001657795402,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity hotthqqb is
  port (ymf : buffer std_logic_vector(4 downto 1); whz : out time);
end hotthqqb;

architecture axvg of hotthqqb is
  
begin
  -- Single-driven assignments
  whz <= 4 us;
  
  -- Multi-driven assignments
  ymf <= ('W', 'H', 'Z', 'X');
  ymf <= ('1', 'H', 'W', '1');
  ymf <= ('1', '-', 'W', 'W');
  ymf <= ('-', '0', '0', 'W');
end axvg;

entity zxlwcv is
  port (owkmnd : linkage real);
end zxlwcv;

library ieee;
use ieee.std_logic_1164.all;

architecture d of zxlwcv is
  signal auqp : time;
  signal juhwjfr : std_logic_vector(4 downto 1);
  signal be : time;
  signal cscsdlzty : std_logic_vector(4 downto 1);
begin
  c : entity work.hotthqqb
    port map (ymf => cscsdlzty, whz => be);
  xiojfzw : entity work.hotthqqb
    port map (ymf => juhwjfr, whz => auqp);
end d;

entity dxfavwy is
  port (mlnn : linkage real; ensfmmxzc : out boolean);
end dxfavwy;

library ieee;
use ieee.std_logic_1164.all;

architecture fpqjptl of dxfavwy is
  signal rrbkm : time;
  signal cjkiigyw : std_logic_vector(4 downto 1);
  signal khnfrh : time;
  signal mpibap : std_logic_vector(4 downto 1);
  signal agqwmvs : time;
  signal ecnrlyc : std_logic_vector(4 downto 1);
begin
  aoxvk : entity work.hotthqqb
    port map (ymf => ecnrlyc, whz => agqwmvs);
  yr : entity work.hotthqqb
    port map (ymf => mpibap, whz => khnfrh);
  ibk : entity work.hotthqqb
    port map (ymf => cjkiigyw, whz => rrbkm);
  
  -- Single-driven assignments
  ensfmmxzc <= TRUE;
  
  -- Multi-driven assignments
  ecnrlyc <= "UWHW";
  ecnrlyc <= "1-H1";
end fpqjptl;



-- Seed after: 7095511717792881486,14652815260262078753
