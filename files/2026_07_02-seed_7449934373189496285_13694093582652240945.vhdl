-- Seed: 7449934373189496285,13694093582652240945

entity tw is
  port (f : buffer real; ebzhjagou : inout time);
end tw;

architecture ykx of tw is
  
begin
  -- Single-driven assignments
  f <= 16#AA.DB7#;
  ebzhjagou <= 2 sec;
end ykx;

entity scdakzxku is
  port (qacbx : in integer_vector(0 to 3));
end scdakzxku;

architecture nuilsllr of scdakzxku is
  signal gnhgu : time;
  signal l : real;
  signal om : time;
  signal tqzcq : real;
begin
  ojishunysc : entity work.tw
    port map (f => tqzcq, ebzhjagou => om);
  reznfvhlj : entity work.tw
    port map (f => l, ebzhjagou => gnhgu);
end nuilsllr;



-- Seed after: 11409132264439555666,13694093582652240945
