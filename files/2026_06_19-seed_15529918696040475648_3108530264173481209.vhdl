-- Seed: 15529918696040475648,3108530264173481209

library ieee;
use ieee.std_logic_1164.all;

entity eqy is
  port (xkrizc : in std_logic_vector(3 to 4); fjbtloxuxy : inout std_logic_vector(2 to 0); pk : in integer; evhksvqpki : buffer integer);
end eqy;

architecture iiwhbbrwky of eqy is
  
begin
  -- Single-driven assignments
  evhksvqpki <= 41041;
  
  -- Multi-driven assignments
  fjbtloxuxy <= (others => '0');
  fjbtloxuxy <= "";
  fjbtloxuxy <= (others => '0');
  fjbtloxuxy <= "";
end iiwhbbrwky;

entity hsripjhit is
  port (d : buffer severity_level);
end hsripjhit;

library ieee;
use ieee.std_logic_1164.all;

architecture l of hsripjhit is
  signal uyqk : std_logic_vector(3 to 4);
  signal keygfnxcv : integer;
  signal dieonijgjl : integer;
  signal coyxxe : integer;
  signal wgnjfplr : std_logic_vector(2 to 0);
  signal gex : std_logic_vector(3 to 4);
begin
  jwiytzxr : entity work.eqy
    port map (xkrizc => gex, fjbtloxuxy => wgnjfplr, pk => coyxxe, evhksvqpki => coyxxe);
  beypkdykim : entity work.eqy
    port map (xkrizc => gex, fjbtloxuxy => wgnjfplr, pk => dieonijgjl, evhksvqpki => keygfnxcv);
  ucx : entity work.eqy
    port map (xkrizc => uyqk, fjbtloxuxy => wgnjfplr, pk => keygfnxcv, evhksvqpki => dieonijgjl);
  
  -- Single-driven assignments
  d <= ERROR;
end l;



-- Seed after: 4849327886354210642,3108530264173481209
