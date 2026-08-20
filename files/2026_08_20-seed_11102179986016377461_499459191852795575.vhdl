-- Seed: 11102179986016377461,499459191852795575

entity utktyvdx is
  port (vhierkhm : buffer boolean_vector(3 downto 0));
end utktyvdx;

architecture cxrqydxbx of utktyvdx is
  
begin
  
end cxrqydxbx;

entity pborbwiqsc is
  port (stxrlmjqhw : buffer severity_level; tux : inout character; yuxddir : buffer integer_vector(4 downto 3); wg : in time);
end pborbwiqsc;

architecture yr of pborbwiqsc is
  
begin
  -- Single-driven assignments
  yuxddir <= (16#2#, 8#401#);
  stxrlmjqhw <= FAILURE;
  tux <= tux;
end yr;

entity afjd is
  port (ktyujmfbi : buffer time; jgcgywmpfs : in time);
end afjd;

architecture kawi of afjd is
  signal jlvrxfsdx : time;
  signal doxgvy : integer_vector(4 downto 3);
  signal fn : character;
  signal ynrnq : severity_level;
  signal iwy : integer_vector(4 downto 3);
  signal rklu : character;
  signal zibkqla : severity_level;
  signal touvfe : boolean_vector(3 downto 0);
begin
  lzlcn : entity work.utktyvdx
    port map (vhierkhm => touvfe);
  avlqz : entity work.pborbwiqsc
    port map (stxrlmjqhw => zibkqla, tux => rklu, yuxddir => iwy, wg => jgcgywmpfs);
  jpnvnshyfy : entity work.pborbwiqsc
    port map (stxrlmjqhw => ynrnq, tux => fn, yuxddir => doxgvy, wg => jlvrxfsdx);
  
  -- Single-driven assignments
  ktyujmfbi <= ktyujmfbi;
  jlvrxfsdx <= 3.0 ms;
end kawi;



-- Seed after: 8764194778221091671,499459191852795575
