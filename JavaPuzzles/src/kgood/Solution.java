package kgood;

import java.util.*;
import java.io.*;
import java.math.*;
import static java.util.stream.Collectors.*;

class Solution {
    static Map<String, Integer> mapa = new HashMap<>();

    public static void main(String args[]) {
        Scanner in = new Scanner(System.in);
//        String S = in.nextLine();
//        int K = in.nextInt();
        String S = "apspbmkwusxrssghyjnboyuuokqcjttiwxsjmejhwjqmqwzmccsbcspwprngkdvuqblkvyzeckxixyeekeruhpckeiupidgjoyjhtubhnbomquiklbqbhllkqdumerxojsjlnrluzitglaifvzvhxkhmhbhqmmqxokqqvecvqxifuheoyzbetswjfsemtztbnrcbmhcolrrvztcmbudoprmwlkepysmlswmabdzjvdvpzglqwgcmscwcrjiienrcllosyvffllashgocoutlyblzwaadqnlqaxinzxivhkjxvsqxaxefazvvocbmnczjzpwcofbqgmprnwhebpwgtocpagiyuqnomccnfrtgievruzktiogfmjdqvsatqzcxlwxvanfujivbweobzvqrexyaevihntwrwakkwpcbgartcgrfyjrlvwdpoosqphvpainvlsxwsnhcqgwooctpjhegouioujklyzuclqvjisdmlkisjoxtrpdyuecdngcxtrlmwwwcmeoaxhnrojwgtjaqvvkfttmrxngaqxptvnxgywamzcdephszqrstweeweiabpmmryexbsjtfmqnvzpblqlrqofbgxbhqyxaiagkznnuvewdgfzzvbbfrnwyyahntpokzrmoyolafgirbkxtajttrrokliyluqazaaajerrqtvwjnzjxpoovfwchgroeicoprhyixtfzichcwjteytumsaezqpuapbhukcfzwthwopxyufhzulzpevfytvonbxqxgapzqcokonxvzfxfxhfovmdqeoneoxnekhmrfxzvewycxjavpleiwiyqoijifedcvjefgyrixnjahbebrwhyjldwffujqzqdqbqactmipiytsqbgawbnpieyqvkfmiwwdrblrawvwzqxsxlwqumhcummcdwhbcyjjckcqlvxhbrkocitazlydtaepmwwsqvuhqjxqjxifajaafehvocumdjqdgk";
        int K = 25;

        int max = 0;
        for (int i = 0; i < S.length(); i++) {
            for (int j = i + K; j <= S.length(); j++) {
                String str = S.substring(i, j);
                int knumber = knumber(str);
                if (knumber == K) {
                    if (j - i > max)
                        max = j - i;
                } else if (knumber > K) {
                    break;
                }
            }
        }
        System.out.println(max);
    }

    static int knumber(String str) {
        mapa.computeIfAbsent(str, s ->
                s.toLowerCase().chars().mapToObj(c -> (char) c)
                        .collect(toSet()).size());
        return mapa.get(str);
    }
}