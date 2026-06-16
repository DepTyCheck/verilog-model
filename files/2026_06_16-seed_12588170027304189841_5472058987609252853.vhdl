-- Seed: 12588170027304189841,5472058987609252853

entity r is
  port (dilrspreql : out bit_vector(1 downto 3); a : linkage bit);
end r;

architecture iro of r is
  
begin
  -- Single-driven assignments
  dilrspreql <= (others => '0');
end iro;

library ieee;
use ieee.std_logic_1164.all;

entity et is
  port (zkeqomzlv : buffer real; eyqx : linkage std_logic_vector(1 downto 0); oynnwr : in time);
end et;

architecture adpyuv of et is
  signal cknczustf : bit;
  signal ebuw : bit_vector(1 downto 3);
  signal rwlhg : bit;
  signal cutl : bit_vector(1 downto 3);
  signal wldoorg : bit;
  signal pqrmioxlls : bit_vector(1 downto 3);
  signal laoirha : bit;
  signal tmk : bit_vector(1 downto 3);
begin
  tluvkzcr : entity work.r
    port map (dilrspreql => tmk, a => laoirha);
  jepxall : entity work.r
    port map (dilrspreql => pqrmioxlls, a => wldoorg);
  chwljdjd : entity work.r
    port map (dilrspreql => cutl, a => rwlhg);
  d : entity work.r
    port map (dilrspreql => ebuw, a => cknczustf);
end adpyuv;

entity inbp is
  port (yxkcqwlk : inout integer; aahdn : out real; ctqczrjllf : in bit);
end inbp;

library ieee;
use ieee.std_logic_1164.all;

architecture qkxbz of inbp is
  signal nq : bit;
  signal twsdezcx : bit_vector(1 downto 3);
  signal kxrfyyw : time;
  signal xkq : std_logic_vector(1 downto 0);
  signal kibaiozuar : real;
  signal gwdpqpt : bit;
  signal xytkt : bit_vector(1 downto 3);
  signal kdugjirj : bit;
  signal kd : bit_vector(1 downto 3);
begin
  p : entity work.r
    port map (dilrspreql => kd, a => kdugjirj);
  ldie : entity work.r
    port map (dilrspreql => xytkt, a => gwdpqpt);
  xqrcs : entity work.et
    port map (zkeqomzlv => kibaiozuar, eyqx => xkq, oynnwr => kxrfyyw);
  bmxdtrnzrt : entity work.r
    port map (dilrspreql => twsdezcx, a => nq);
  
  -- Single-driven assignments
  aahdn <= 4.1;
  kxrfyyw <= 16#F_C# ps;
  yxkcqwlk <= 8#21465#;
  
  -- Multi-driven assignments
  xkq <= ('L', '0');
end qkxbz;

entity zkoagxbo is
  port (qrxnnzugg : buffer time);
end zkoagxbo;

library ieee;
use ieee.std_logic_1164.all;

architecture fihq of zkoagxbo is
  signal ytcwomgez : bit;
  signal jkdvuduwt : bit_vector(1 downto 3);
  signal zjgy : std_logic_vector(1 downto 0);
  signal wolzjfis : real;
begin
  du : entity work.et
    port map (zkeqomzlv => wolzjfis, eyqx => zjgy, oynnwr => qrxnnzugg);
  wptnjqwrhu : entity work.r
    port map (dilrspreql => jkdvuduwt, a => ytcwomgez);
  
  -- Single-driven assignments
  qrxnnzugg <= 16#BD.C6# ps;
  
  -- Multi-driven assignments
  zjgy <= ('Z', '0');
  zjgy <= ('U', 'X');
  zjgy <= ('L', 'W');
  zjgy <= "ZZ";
end fihq;



-- Seed after: 5673905326039953453,5472058987609252853
