-- Seed: 17062458579513004386,3108530264173481209

entity raizsj is
  port (ybijsfcp : out boolean_vector(4 downto 4); irnoyr : out time);
end raizsj;

architecture cvmy of raizsj is
  
begin
  -- Single-driven assignments
  irnoyr <= 242.233 fs;
  ybijsfcp <= (others => FALSE);
end cvmy;

entity ecxdph is
  port (bfmcvavz : out real);
end ecxdph;

architecture kthrwgxgjq of ecxdph is
  signal ayawrh : time;
  signal y : boolean_vector(4 downto 4);
begin
  zkuyehs : entity work.raizsj
    port map (ybijsfcp => y, irnoyr => ayawrh);
  
  -- Single-driven assignments
  bfmcvavz <= 21.3;
end kthrwgxgjq;

entity veze is
  port (k : out time; mhquth : linkage time; piuxyq : inout severity_level);
end veze;

architecture mzs of veze is
  signal nfteyrzl : time;
  signal z : boolean_vector(4 downto 4);
  signal fdikzujwyo : real;
begin
  xamgg : entity work.ecxdph
    port map (bfmcvavz => fdikzujwyo);
  zomddfhz : entity work.raizsj
    port map (ybijsfcp => z, irnoyr => nfteyrzl);
  
  -- Single-driven assignments
  piuxyq <= FAILURE;
  k <= 16#88.5_4_0_A_F# fs;
end mzs;



-- Seed after: 16232671531532132903,3108530264173481209
