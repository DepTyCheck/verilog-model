-- Seed: 13252968331896391615,14629254427735353553

entity hsxtdwvvd is
  port (pltgpzypt : in string(4 to 1); lnobyqikv : inout string(2 downto 4); ltoq : out boolean; xoz : buffer time);
end hsxtdwvvd;

architecture yc of hsxtdwvvd is
  
begin
  
end yc;

entity xbodxq is
  port (mnccdpdk : out real; hgzqwub : inout integer_vector(3 downto 3); ifujgpk : buffer bit);
end xbodxq;

architecture fbimrqtkc of xbodxq is
  signal az : time;
  signal dvhudg : boolean;
  signal g : string(2 downto 4);
  signal kjmjcmdlym : time;
  signal rvis : boolean;
  signal sgw : string(4 to 1);
  signal cvadkxb : time;
  signal wz : boolean;
  signal yfzspixqvp : string(2 downto 4);
  signal si : time;
  signal ixvjoayzn : boolean;
  signal dcocj : string(2 downto 4);
  signal mgmqgcm : string(4 to 1);
begin
  kpsgp : entity work.hsxtdwvvd
    port map (pltgpzypt => mgmqgcm, lnobyqikv => dcocj, ltoq => ixvjoayzn, xoz => si);
  ww : entity work.hsxtdwvvd
    port map (pltgpzypt => mgmqgcm, lnobyqikv => yfzspixqvp, ltoq => wz, xoz => cvadkxb);
  xoa : entity work.hsxtdwvvd
    port map (pltgpzypt => sgw, lnobyqikv => mgmqgcm, ltoq => rvis, xoz => kjmjcmdlym);
  motnhepon : entity work.hsxtdwvvd
    port map (pltgpzypt => mgmqgcm, lnobyqikv => g, ltoq => dvhudg, xoz => az);
  
  -- Single-driven assignments
  ifujgpk <= '0';
  mnccdpdk <= 311.4;
  hgzqwub <= (others => 8#61#);
  sgw <= (others => ' ');
end fbimrqtkc;

entity pcggxval is
  port (h : buffer time; rdjhmsti : linkage real);
end pcggxval;

architecture jhcivrymwx of pcggxval is
  
begin
  -- Single-driven assignments
  h <= 2 sec;
end jhcivrymwx;



-- Seed after: 13350481218824323689,14629254427735353553
