-- Seed: 17667055104866656622,5444032936641997477

library ieee;
use ieee.std_logic_1164.all;

entity upucsx is
  port (sqzs : buffer std_logic_vector(2 downto 1); mnvuuwd : linkage character);
end upucsx;

architecture ocpa of upucsx is
  
begin
  -- Multi-driven assignments
  sqzs <= ('H', '-');
end ocpa;

entity uo is
  port (jhh : out real; ebmmcyokzo : in integer);
end uo;

library ieee;
use ieee.std_logic_1164.all;

architecture jmicz of uo is
  signal sminqvml : character;
  signal vrgmef : character;
  signal an : std_logic_vector(2 downto 1);
begin
  ooqexl : entity work.upucsx
    port map (sqzs => an, mnvuuwd => vrgmef);
  lpz : entity work.upucsx
    port map (sqzs => an, mnvuuwd => sminqvml);
  
  -- Single-driven assignments
  jhh <= 16#C_5_A_7.E#;
  
  -- Multi-driven assignments
  an <= ('L', 'X');
  an <= "Z1";
end jmicz;

entity nq is
  port (irwcdtlft : buffer integer);
end nq;

library ieee;
use ieee.std_logic_1164.all;

architecture e of nq is
  signal ovgzf : character;
  signal czczje : real;
  signal areqiyic : character;
  signal zawrduj : std_logic_vector(2 downto 1);
  signal oxl : real;
begin
  lkha : entity work.uo
    port map (jhh => oxl, ebmmcyokzo => irwcdtlft);
  kmqomof : entity work.upucsx
    port map (sqzs => zawrduj, mnvuuwd => areqiyic);
  ogyjlzoxw : entity work.uo
    port map (jhh => czczje, ebmmcyokzo => irwcdtlft);
  qmunfixkr : entity work.upucsx
    port map (sqzs => zawrduj, mnvuuwd => ovgzf);
  
  -- Single-driven assignments
  irwcdtlft <= 2#00111#;
  
  -- Multi-driven assignments
  zawrduj <= ('L', 'L');
end e;



-- Seed after: 7927980354101261659,5444032936641997477
