-- Seed: 1728655488026403005,6869379746012019351

entity jbfc is
  port (syrducsxs : linkage severity_level; owslwjzuha : buffer integer_vector(4 to 1); lfhc : in integer; nsh : buffer time);
end jbfc;

architecture hnyzlnnoi of jbfc is
  
begin
  -- Single-driven assignments
  nsh <= nsh;
end hnyzlnnoi;

library ieee;
use ieee.std_logic_1164.all;

entity e is
  port (kecl : buffer std_logic);
end e;

architecture z of e is
  
begin
  -- Multi-driven assignments
  kecl <= kecl;
end z;

entity sytv is
  port (ghuxzzxm : in real; qwcueb : in integer_vector(3 downto 3); rw : in time; mkgrenmoa : buffer character);
end sytv;

library ieee;
use ieee.std_logic_1164.all;

architecture ejnors of sytv is
  signal twmamb : time;
  signal dlarvquw : integer_vector(4 to 1);
  signal zmiomle : severity_level;
  signal mofdxebhp : time;
  signal vmzfi : integer;
  signal ivrapocjq : integer_vector(4 to 1);
  signal hawmq : severity_level;
  signal tj : std_logic;
begin
  jnimgijk : entity work.e
    port map (kecl => tj);
  hbrfchdpck : entity work.jbfc
    port map (syrducsxs => hawmq, owslwjzuha => ivrapocjq, lfhc => vmzfi, nsh => mofdxebhp);
  ptdlx : entity work.jbfc
    port map (syrducsxs => zmiomle, owslwjzuha => dlarvquw, lfhc => vmzfi, nsh => twmamb);
  
  -- Single-driven assignments
  mkgrenmoa <= 'q';
  
  -- Multi-driven assignments
  tj <= tj;
end ejnors;



-- Seed after: 8857956279492630614,6869379746012019351
