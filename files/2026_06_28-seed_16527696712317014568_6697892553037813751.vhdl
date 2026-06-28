-- Seed: 16527696712317014568,6697892553037813751

entity lgedqz is
  port (s : buffer bit_vector(2 to 0));
end lgedqz;

architecture xv of lgedqz is
  
begin
  -- Single-driven assignments
  s <= (others => '0');
end xv;

entity wl is
  port (zmer : inout integer);
end wl;

architecture zkzsndny of wl is
  signal hljufhlpnk : bit_vector(2 to 0);
  signal npkbpfoez : bit_vector(2 to 0);
  signal gmh : bit_vector(2 to 0);
begin
  wjocl : entity work.lgedqz
    port map (s => gmh);
  enkmqqf : entity work.lgedqz
    port map (s => npkbpfoez);
  coi : entity work.lgedqz
    port map (s => hljufhlpnk);
end zkzsndny;

entity zxenbvok is
  port (pch : out time; okwqytrg : linkage real; bd : buffer time; xwgw : linkage time);
end zxenbvok;

architecture wadpyhiu of zxenbvok is
  signal ahnidbt : bit_vector(2 to 0);
begin
  bco : entity work.lgedqz
    port map (s => ahnidbt);
  
  -- Single-driven assignments
  pch <= 1 min;
  bd <= 8#23.61214# ps;
end wadpyhiu;



-- Seed after: 11891539153545906269,6697892553037813751
