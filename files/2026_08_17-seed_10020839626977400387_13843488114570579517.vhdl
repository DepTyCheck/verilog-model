-- Seed: 10020839626977400387,13843488114570579517

entity heqos is
  port (wqeji : inout real);
end heqos;

architecture rgzxefoopb of heqos is
  
begin
  -- Single-driven assignments
  wqeji <= wqeji;
end rgzxefoopb;

library ieee;
use ieee.std_logic_1164.all;

entity sfasmg is
  port (wtbitoinl : inout std_logic_vector(4 to 3); h : buffer bit_vector(2 downto 4));
end sfasmg;

architecture vzpbedncl of sfasmg is
  signal mrfkqy : real;
  signal ysmuqpqk : real;
  signal saesqqhuu : real;
  signal gdqazlui : real;
begin
  xadufnrbr : entity work.heqos
    port map (wqeji => gdqazlui);
  rihvi : entity work.heqos
    port map (wqeji => saesqqhuu);
  vxf : entity work.heqos
    port map (wqeji => ysmuqpqk);
  iwrhqwscfx : entity work.heqos
    port map (wqeji => mrfkqy);
  
  -- Multi-driven assignments
  wtbitoinl <= wtbitoinl;
end vzpbedncl;

library ieee;
use ieee.std_logic_1164.all;

entity qqcxuff is
  port (csewps : in std_logic_vector(0 downto 1));
end qqcxuff;

library ieee;
use ieee.std_logic_1164.all;

architecture toqapgsizq of qqcxuff is
  signal eer : bit_vector(2 downto 4);
  signal mcleiqg : std_logic_vector(4 to 3);
  signal cploc : real;
begin
  kkhzzzgdes : entity work.heqos
    port map (wqeji => cploc);
  dbtyuylr : entity work.sfasmg
    port map (wtbitoinl => mcleiqg, h => eer);
  
  -- Multi-driven assignments
  mcleiqg <= (others => '0');
  mcleiqg <= csewps;
end toqapgsizq;

entity cvwtaskg is
  port (qaklambp : inout time; ewtbceo : in time);
end cvwtaskg;

library ieee;
use ieee.std_logic_1164.all;

architecture zv of cvwtaskg is
  signal zw : real;
  signal ghwjq : std_logic_vector(0 downto 1);
begin
  upyzpoxa : entity work.qqcxuff
    port map (csewps => ghwjq);
  qeukhz : entity work.heqos
    port map (wqeji => zw);
  
  -- Single-driven assignments
  qaklambp <= 0 sec;
end zv;



-- Seed after: 14011808265383123698,13843488114570579517
