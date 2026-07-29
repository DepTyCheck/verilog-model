-- Seed: 10161391506551798715,14641901754878719179

entity vmdsp is
  port (stzocscnfc : in time; r : inout time);
end vmdsp;

architecture ctsnbilr of vmdsp is
  
begin
  -- Single-driven assignments
  r <= r;
end ctsnbilr;

entity fwvay is
  port (rvigzowk : in time; u : buffer time; uekf : inout integer_vector(3 downto 1));
end fwvay;

architecture kb of fwvay is
  
begin
  -- Single-driven assignments
  uekf <= uekf;
  u <= u;
end kb;

entity micmiw is
  port (zhmjezsvn : out character; gqqobscncj : out time_vector(1 to 0));
end micmiw;

architecture ks of micmiw is
  signal hj : integer_vector(3 downto 1);
  signal xi : time;
  signal ychthqapr : time;
  signal xredhntkma : time;
  signal onioqnjuey : time;
  signal rsgl : time;
begin
  x : entity work.vmdsp
    port map (stzocscnfc => rsgl, r => onioqnjuey);
  njwnwi : entity work.vmdsp
    port map (stzocscnfc => xredhntkma, r => ychthqapr);
  lycbm : entity work.fwvay
    port map (rvigzowk => xi, u => xredhntkma, uekf => hj);
  
  -- Single-driven assignments
  rsgl <= 1 hr;
  xi <= xi;
  zhmjezsvn <= zhmjezsvn;
  gqqobscncj <= gqqobscncj;
end ks;



-- Seed after: 10693392529513210730,14641901754878719179
