-- Seed: 17582447287957397230,12011142928354116943

entity gvggjtde is
  port (wmw : in integer);
end gvggjtde;

architecture lwxn of gvggjtde is
  
begin
  
end lwxn;

entity kbivceirg is
  port (odf : inout integer; hxly : out bit_vector(3 downto 2));
end kbivceirg;

architecture opjjavw of kbivceirg is
  signal srrneujyuc : integer;
  signal f : integer;
begin
  zjsoaolpzv : entity work.gvggjtde
    port map (wmw => f);
  jcs : entity work.gvggjtde
    port map (wmw => srrneujyuc);
  tqcyilsgu : entity work.gvggjtde
    port map (wmw => odf);
  
  -- Single-driven assignments
  f <= 16#1_F#;
  srrneujyuc <= 8#4#;
  odf <= 12;
  hxly <= ('0', '0');
end opjjavw;

library ieee;
use ieee.std_logic_1164.all;

entity u is
  port (e : out std_logic_vector(0 to 4); uxrfgijae : out time);
end u;

architecture nwjn of u is
  signal wc : integer;
begin
  fmsfoahd : entity work.gvggjtde
    port map (wmw => wc);
  
  -- Multi-driven assignments
  e <= ('U', 'U', 'Z', '-', '0');
end nwjn;



-- Seed after: 10254408138958797851,12011142928354116943
