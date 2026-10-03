-- Seed: 3029111905345846766,6140041381800297705

entity yfmqjtalbw is
  port (pihltb : in severity_level; yiylayubj : in integer_vector(3 downto 2); mfdzooepzm : in bit);
end yfmqjtalbw;

architecture cwbexg of yfmqjtalbw is
  
begin
  
end cwbexg;

entity bns is
  port (xcx : in bit);
end bns;

architecture hlxkiupaf of bns is
  signal bjbi : severity_level;
  signal qirzajyrjj : integer_vector(3 downto 2);
  signal lzyriykdql : bit;
  signal metaundhh : integer_vector(3 downto 2);
  signal hswfuhmgbw : severity_level;
begin
  vzxwvuz : entity work.yfmqjtalbw
    port map (pihltb => hswfuhmgbw, yiylayubj => metaundhh, mfdzooepzm => lzyriykdql);
  z : entity work.yfmqjtalbw
    port map (pihltb => hswfuhmgbw, yiylayubj => qirzajyrjj, mfdzooepzm => xcx);
  enovqvmq : entity work.yfmqjtalbw
    port map (pihltb => bjbi, yiylayubj => metaundhh, mfdzooepzm => lzyriykdql);
  
  -- Single-driven assignments
  qirzajyrjj <= metaundhh;
  bjbi <= NOTE;
  lzyriykdql <= '0';
end hlxkiupaf;



-- Seed after: 4713596957909569201,6140041381800297705
