-- Seed: 3354178593606863325,7304262412290825129

entity s is
  port (yrupdn : in boolean_vector(0 downto 4));
end s;

architecture cagmoqhs of s is
  
begin
  
end cagmoqhs;

entity qkjzpxu is
  port (fv : out time_vector(4 downto 0); f : linkage character; inkgocnimt : buffer boolean_vector(4 downto 0));
end qkjzpxu;

architecture wofju of qkjzpxu is
  signal btj : boolean_vector(0 downto 4);
  signal wqsy : boolean_vector(0 downto 4);
begin
  itgwwhsnr : entity work.s
    port map (yrupdn => wqsy);
  pbjtptols : entity work.s
    port map (yrupdn => btj);
  dat : entity work.s
    port map (yrupdn => wqsy);
  xazhnq : entity work.s
    port map (yrupdn => wqsy);
  
  -- Single-driven assignments
  inkgocnimt <= inkgocnimt;
end wofju;

entity xblklqffoj is
  port (pd : inout character; rxklviag : linkage time; gzijmldayi : out boolean);
end xblklqffoj;

architecture qxbkjsgz of xblklqffoj is
  signal cwarqpfp : boolean_vector(0 downto 4);
  signal dmjb : boolean_vector(4 downto 0);
  signal hnnire : time_vector(4 downto 0);
  signal q : boolean_vector(0 downto 4);
begin
  p : entity work.s
    port map (yrupdn => q);
  bpo : entity work.qkjzpxu
    port map (fv => hnnire, f => pd, inkgocnimt => dmjb);
  jnmildkxzu : entity work.s
    port map (yrupdn => q);
  rdbvcn : entity work.s
    port map (yrupdn => cwarqpfp);
  
  -- Single-driven assignments
  cwarqpfp <= (others => TRUE);
  gzijmldayi <= gzijmldayi;
end qxbkjsgz;

entity kg is
  port (iestctd : linkage time);
end kg;

architecture q of kg is
  
begin
  
end q;



-- Seed after: 12933928853819114083,7304262412290825129
