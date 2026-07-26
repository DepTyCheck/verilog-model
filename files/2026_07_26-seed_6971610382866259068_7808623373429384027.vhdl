-- Seed: 6971610382866259068,7808623373429384027

entity khxpamwe is
  port (ennwttry : in boolean);
end khxpamwe;

architecture lpfl of khxpamwe is
  
begin
  
end lpfl;

library ieee;
use ieee.std_logic_1164.all;

entity kn is
  port (mgocsz : buffer integer; rhrnzxalqs : out time_vector(3 downto 0); bismja : in integer; gjrvtkxslv : in std_logic_vector(2 downto 2));
end kn;

architecture ffkpptwd of kn is
  signal gvl : boolean;
  signal dtkqejy : boolean;
begin
  lalfqd : entity work.khxpamwe
    port map (ennwttry => dtkqejy);
  j : entity work.khxpamwe
    port map (ennwttry => gvl);
end ffkpptwd;

entity xy is
  port (e : buffer integer);
end xy;

library ieee;
use ieee.std_logic_1164.all;

architecture dhnyeuv of xy is
  signal zngzmi : std_logic_vector(2 downto 2);
  signal luamuueubx : integer;
  signal zu : time_vector(3 downto 0);
  signal klzczm : integer;
  signal jnryedjwy : std_logic_vector(2 downto 2);
  signal iitng : integer;
  signal ylovpx : time_vector(3 downto 0);
  signal fkvcgnq : integer;
begin
  s : entity work.kn
    port map (mgocsz => fkvcgnq, rhrnzxalqs => ylovpx, bismja => iitng, gjrvtkxslv => jnryedjwy);
  hygz : entity work.kn
    port map (mgocsz => klzczm, rhrnzxalqs => zu, bismja => luamuueubx, gjrvtkxslv => zngzmi);
end dhnyeuv;

entity ddsjl is
  port (bwsalrlyni : buffer time);
end ddsjl;

architecture ceenlh of ddsjl is
  signal zaqh : boolean;
  signal jqijznfme : boolean;
begin
  hzjhcdxcee : entity work.khxpamwe
    port map (ennwttry => jqijznfme);
  q : entity work.khxpamwe
    port map (ennwttry => zaqh);
  pe : entity work.khxpamwe
    port map (ennwttry => jqijznfme);
  
  -- Single-driven assignments
  bwsalrlyni <= 10143 us;
  zaqh <= FALSE;
  jqijznfme <= jqijznfme;
end ceenlh;



-- Seed after: 14371883020044311370,7808623373429384027
