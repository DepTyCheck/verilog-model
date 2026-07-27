-- Seed: 4470362446128284397,662889661651915549

library ieee;
use ieee.std_logic_1164.all;

entity e is
  port (d : linkage integer; qu : in time; yhmhlsqvvh : out std_logic_vector(4 downto 1); dphddhaz : buffer std_logic_vector(4 to 1));
end e;

architecture wehhxbnhew of e is
  
begin
  -- Multi-driven assignments
  dphddhaz <= (others => '0');
  dphddhaz <= dphddhaz;
  dphddhaz <= dphddhaz;
  dphddhaz <= dphddhaz;
end wehhxbnhew;

entity itmttquyx is
  port (jxfoahard : out real_vector(2 to 0));
end itmttquyx;

library ieee;
use ieee.std_logic_1164.all;

architecture hvx of itmttquyx is
  signal nsohyewa : std_logic_vector(4 to 1);
  signal idn : std_logic_vector(4 downto 1);
  signal nerf : time;
  signal oxgsmmsds : integer;
begin
  pisba : entity work.e
    port map (d => oxgsmmsds, qu => nerf, yhmhlsqvvh => idn, dphddhaz => nsohyewa);
  
  -- Single-driven assignments
  nerf <= nerf;
  jxfoahard <= jxfoahard;
end hvx;

entity qpsqokgx is
  port (qat : out time; pzqwxta : out time);
end qpsqokgx;

library ieee;
use ieee.std_logic_1164.all;

architecture grtvay of qpsqokgx is
  signal soesvn : std_logic_vector(4 downto 1);
  signal zy : time;
  signal r : integer;
  signal jimgrwjmz : real_vector(2 to 0);
  signal gzuqvstfo : std_logic_vector(4 to 1);
  signal jahgfmbqia : std_logic_vector(4 downto 1);
  signal ves : integer;
  signal dmnhdoj : real_vector(2 to 0);
begin
  cbmpi : entity work.itmttquyx
    port map (jxfoahard => dmnhdoj);
  ggoteibab : entity work.e
    port map (d => ves, qu => pzqwxta, yhmhlsqvvh => jahgfmbqia, dphddhaz => gzuqvstfo);
  ocmxgvzr : entity work.itmttquyx
    port map (jxfoahard => jimgrwjmz);
  zijw : entity work.e
    port map (d => r, qu => zy, yhmhlsqvvh => soesvn, dphddhaz => gzuqvstfo);
  
  -- Single-driven assignments
  pzqwxta <= qat;
  qat <= pzqwxta;
  zy <= 8#5# ns;
  
  -- Multi-driven assignments
  soesvn <= "WH-Z";
end grtvay;



-- Seed after: 16429252434133582270,662889661651915549
