-- Seed: 2554760844336381663,13501862637168280927

library ieee;
use ieee.std_logic_1164.all;

entity iiwbp is
  port (d : in time; kybxdkp : inout std_logic_vector(4 downto 3); qtbwizhud : out std_logic_vector(4 downto 0));
end iiwbp;

architecture hfh of iiwbp is
  
begin
  -- Multi-driven assignments
  qtbwizhud <= "WHH-Z";
  qtbwizhud <= qtbwizhud;
  qtbwizhud <= qtbwizhud;
  qtbwizhud <= "-HXUH";
end hfh;

entity piclfj is
  port (brte : buffer integer_vector(1 downto 2); v : buffer real; kghkeqsma : out bit);
end piclfj;

library ieee;
use ieee.std_logic_1164.all;

architecture owo of piclfj is
  signal iwa : std_logic_vector(4 downto 0);
  signal abu : std_logic_vector(4 downto 3);
  signal gfkye : std_logic_vector(4 downto 0);
  signal ouhp : std_logic_vector(4 downto 3);
  signal jeuegzxt : time;
begin
  kwfs : entity work.iiwbp
    port map (d => jeuegzxt, kybxdkp => ouhp, qtbwizhud => gfkye);
  ex : entity work.iiwbp
    port map (d => jeuegzxt, kybxdkp => ouhp, qtbwizhud => gfkye);
  xllz : entity work.iiwbp
    port map (d => jeuegzxt, kybxdkp => ouhp, qtbwizhud => gfkye);
  rgeca : entity work.iiwbp
    port map (d => jeuegzxt, kybxdkp => abu, qtbwizhud => iwa);
  
  -- Single-driven assignments
  jeuegzxt <= jeuegzxt;
  brte <= brte;
  v <= v;
  kghkeqsma <= kghkeqsma;
  
  -- Multi-driven assignments
  abu <= ouhp;
  ouhp <= ouhp;
end owo;

entity jgbygnz is
  port (evcpyf : buffer real; uj : out real; uxwd : inout time);
end jgbygnz;

architecture gkdvrnfhnc of jgbygnz is
  signal naerdf : bit;
  signal fkgycmxer : integer_vector(1 downto 2);
begin
  lpdbrxnuy : entity work.piclfj
    port map (brte => fkgycmxer, v => uj, kghkeqsma => naerdf);
  
  -- Single-driven assignments
  uxwd <= 1 min;
  evcpyf <= uj;
end gkdvrnfhnc;



-- Seed after: 4301587960261731671,13501862637168280927
