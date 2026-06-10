-- Seed: 2798690013119206664,5415160250146859793



entity gbhsaor is
  port (sbpoan : linkage bit_vector(1 to 4); w : in real; blte : inout real; tkbab : out time);
end gbhsaor;



architecture jo of gbhsaor is
  
begin
  
end jo;



entity jxqno is
  port (lrifiekv : inout real_vector(2 to 1); dea : buffer real);
end jxqno;



architecture sx of jxqno is
  signal u : time;
  signal vddafba : bit_vector(1 to 4);
  signal zk : time;
  signal yhksmd : real;
  signal dxxvmyi : bit_vector(1 to 4);
begin
  pcgv : entity work.gbhsaor
    port map (sbpoan => dxxvmyi, w => yhksmd, blte => yhksmd, tkbab => zk);
  nkcoxn : entity work.gbhsaor
    port map (sbpoan => vddafba, w => dea, blte => dea, tkbab => u);
end sx;



entity ys is
  port (bjlgqgp : buffer real; bjttahr : inout boolean);
end ys;



architecture rbdgexptp of ys is
  signal bm : time;
  signal oepfplrot : real;
  signal irlun : bit_vector(1 to 4);
  signal afohug : real;
  signal pa : real_vector(2 to 1);
begin
  x : entity work.jxqno
    port map (lrifiekv => pa, dea => afohug);
  nf : entity work.gbhsaor
    port map (sbpoan => irlun, w => oepfplrot, blte => bjlgqgp, tkbab => bm);
end rbdgexptp;



-- Seed after: 2962827979585267976,5415160250146859793
