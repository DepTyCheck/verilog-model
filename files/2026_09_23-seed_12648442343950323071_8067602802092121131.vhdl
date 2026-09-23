-- Seed: 12648442343950323071,8067602802092121131

entity neaansedi is
  port (gk : buffer time; pkxacjdyhr : in boolean_vector(0 downto 3));
end neaansedi;

architecture toosm of neaansedi is
  
begin
  -- Single-driven assignments
  gk <= gk;
end toosm;

entity jumqlyof is
  port (rfpeip : inout bit);
end jumqlyof;

architecture vmlqqijz of jumqlyof is
  
begin
  -- Single-driven assignments
  rfpeip <= '1';
end vmlqqijz;

entity cgdqyuko is
  port (cfzgd : buffer time);
end cgdqyuko;

architecture liumiz of cgdqyuko is
  
begin
  -- Single-driven assignments
  cfzgd <= 223.0_3 ms;
end liumiz;

entity xbw is
  port (ljmpmq : in real; jogcgaagt : inout time);
end xbw;

architecture fagymu of xbw is
  signal saz : boolean_vector(0 downto 3);
  signal u : time;
  signal gefg : boolean_vector(0 downto 3);
  signal nd : time;
  signal rnltcrr : bit;
  signal ss : boolean_vector(0 downto 3);
begin
  idgnzjji : entity work.neaansedi
    port map (gk => jogcgaagt, pkxacjdyhr => ss);
  q : entity work.jumqlyof
    port map (rfpeip => rnltcrr);
  lu : entity work.neaansedi
    port map (gk => nd, pkxacjdyhr => gefg);
  nvefytqmrn : entity work.neaansedi
    port map (gk => u, pkxacjdyhr => saz);
  
  -- Single-driven assignments
  ss <= (others => TRUE);
  gefg <= (others => TRUE);
end fagymu;



-- Seed after: 10166415814949631982,8067602802092121131
