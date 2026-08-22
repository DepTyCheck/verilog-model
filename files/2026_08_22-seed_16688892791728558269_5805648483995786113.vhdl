-- Seed: 16688892791728558269,5805648483995786113

library ieee;
use ieee.std_logic_1164.all;

entity aka is
  port (ejaap : in bit_vector(2 downto 2); s : out real; wzixjlcjo : out std_logic_vector(0 to 1));
end aka;

architecture rb of aka is
  
begin
  -- Multi-driven assignments
  wzixjlcjo <= ('H', 'L');
  wzixjlcjo <= wzixjlcjo;
  wzixjlcjo <= wzixjlcjo;
  wzixjlcjo <= wzixjlcjo;
end rb;

library ieee;
use ieee.std_logic_1164.all;

entity kl is
  port (nxobzynrf : buffer std_logic_vector(4 downto 3); c : inout character; rt : linkage time_vector(1 to 2));
end kl;

library ieee;
use ieee.std_logic_1164.all;

architecture yvw of kl is
  signal zzmbyqs : std_logic_vector(0 to 1);
  signal fu : real;
  signal jdbjv : std_logic_vector(0 to 1);
  signal jfw : real;
  signal habppzkogh : bit_vector(2 downto 2);
  signal pkkapebxa : std_logic_vector(0 to 1);
  signal wdlrydrqd : real;
  signal gid : real;
  signal oz : bit_vector(2 downto 2);
begin
  qkbuqnmv : entity work.aka
    port map (ejaap => oz, s => gid, wzixjlcjo => nxobzynrf);
  nqq : entity work.aka
    port map (ejaap => oz, s => wdlrydrqd, wzixjlcjo => pkkapebxa);
  ajfjvcm : entity work.aka
    port map (ejaap => habppzkogh, s => jfw, wzixjlcjo => jdbjv);
  wxyqnsfv : entity work.aka
    port map (ejaap => oz, s => fu, wzixjlcjo => zzmbyqs);
  
  -- Single-driven assignments
  habppzkogh <= oz;
  
  -- Multi-driven assignments
  nxobzynrf <= nxobzynrf;
end yvw;



-- Seed after: 4451367600317557830,5805648483995786113
